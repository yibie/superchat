import Foundation
import LinkPresentation
import Observation

@MainActor
@Observable
final class LinkMetadataService {
    private(set) var cache: [String: LinkMetadata] = [:]
    private var cacheOrder: [String] = []
    private var inFlight: Set<String> = []
    private var queued: [String] = []
    private var queuedSet: Set<String> = []
    private var callbacks: [String: [(LinkMetadata) -> Void]] = [:]
    private let maxConcurrent = 3
    private let maxCacheEntries = 256

    func cached(_ urlString: String) -> LinkMetadata? {
        cache[urlString]
    }

    func fetchIfNeeded(_ urlString: String, onComplete: @escaping (LinkMetadata) -> Void) {
        if let cached = cache[urlString] {
            onComplete(cached)
            return
        }

        callbacks[urlString, default: []].append(onComplete)

        guard !inFlight.contains(urlString),
              !queuedSet.contains(urlString),
              let url = URL(string: urlString) else { return }

        // Detect direct image URLs
        let ext = url.pathExtension.lowercased()
        if ["png", "jpg", "jpeg", "gif", "webp", "svg"].contains(ext) {
            let meta = LinkMetadata(imageURL: urlString, contentType: .image)
            resolve(urlString, with: meta)
            return
        }

        // Detect video platforms before fetching
        if let videoMeta = detectVideoMeta(url) {
            resolve(urlString, with: videoMeta)
            return
        }

        queued.append(urlString)
        queuedSet.insert(urlString)
        drainQueue()
    }

    private func drainQueue() {
        while inFlight.count < maxConcurrent, let nextURL = queued.first {
            queued.removeFirst()
            queuedSet.remove(nextURL)
            startFetch(nextURL)
        }
    }

    private func startFetch(_ urlString: String) {
        guard let url = URL(string: urlString) else { return }

        inFlight.insert(urlString)
        let provider = LPMetadataProvider()
        let hostString = url.host()
        provider.startFetchingMetadata(for: url) { [weak self] lpMeta, _ in
            // Extract Sendable values before crossing isolation boundary
            let title = lpMeta?.title
            let hasImage = lpMeta?.imageProvider != nil
            let siteName = lpMeta?.url?.host() ?? hostString
            Task { @MainActor [weak self] in
                guard let self else { return }
                let meta = LinkMetadata(
                    metaTitle: title,
                    metaDescription: nil,
                    imageURL: hasImage ? urlString : nil,
                    siteName: siteName,
                    contentType: .webpage
                )
                self.resolve(urlString, with: meta)
            }
        }
    }

    private func resolve(_ urlString: String, with metadata: LinkMetadata) {
        inFlight.remove(urlString)
        insertIntoCache(metadata, for: urlString)
        let pending = callbacks.removeValue(forKey: urlString) ?? []
        for callback in pending {
            callback(metadata)
        }
        drainQueue()
    }

    private func insertIntoCache(_ metadata: LinkMetadata, for urlString: String) {
        cache[urlString] = metadata
        cacheOrder.removeAll { $0 == urlString }
        cacheOrder.append(urlString)

        while cacheOrder.count > maxCacheEntries {
            let evicted = cacheOrder.removeFirst()
            cache.removeValue(forKey: evicted)
        }
    }

    private func detectVideoMeta(_ url: URL) -> LinkMetadata? {
        let host = url.host()?.lowercased() ?? ""

        // YouTube
        if host.contains("youtube.com") || host.contains("youtu.be") {
            if let videoID = extractYouTubeID(url) {
                return LinkMetadata(
                    imageURL: "https://img.youtube.com/vi/\(videoID)/mqdefault.jpg",
                    siteName: "YouTube",
                    videoID: videoID,
                    contentType: .video
                )
            }
        }

        // Vimeo
        if host.contains("vimeo.com") {
            let pathComponents = url.pathComponents.filter { $0 != "/" }
            if let videoID = pathComponents.first(where: { $0.allSatisfy(\.isNumber) }) {
                return LinkMetadata(
                    siteName: "Vimeo",
                    videoID: videoID,
                    contentType: .video
                )
            }
        }

        return nil
    }

    private func extractYouTubeID(_ url: URL) -> String? {
        // youtu.be/VIDEO_ID
        if url.host()?.contains("youtu.be") == true {
            let id = url.pathComponents.filter { $0 != "/" }.first
            return id?.isEmpty == false ? id : nil
        }
        // youtube.com/watch?v=VIDEO_ID
        let components = URLComponents(url: url, resolvingAgainstBaseURL: false)
        return components?.queryItems?.first(where: { $0.name == "v" })?.value
    }
}
