import Foundation
import LinkPresentation
import Observation

@MainActor
@Observable
final class LinkMetadataService {
    private(set) var cache: [String: LinkMetadata] = [:]
    private var inFlight: Set<String> = []
    private let maxConcurrent = 3

    func cached(_ urlString: String) -> LinkMetadata? {
        cache[urlString]
    }

    func fetchIfNeeded(_ urlString: String, onComplete: @escaping (LinkMetadata) -> Void) {
        guard cache[urlString] == nil,
              !inFlight.contains(urlString),
              inFlight.count < maxConcurrent,
              let url = URL(string: urlString) else { return }

        // Detect direct image URLs
        let ext = url.pathExtension.lowercased()
        if ["png", "jpg", "jpeg", "gif", "webp", "svg"].contains(ext) {
            let meta = LinkMetadata(imageURL: urlString, contentType: .image)
            cache[urlString] = meta
            onComplete(meta)
            return
        }

        // Detect video platforms before fetching
        if let videoMeta = detectVideoMeta(url) {
            cache[urlString] = videoMeta
            onComplete(videoMeta)
            return
        }

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
                self.inFlight.remove(urlString)
                let meta = LinkMetadata(
                    metaTitle: title,
                    metaDescription: nil,
                    imageURL: hasImage ? urlString : nil,
                    siteName: siteName,
                    contentType: .webpage
                )
                self.cache[urlString] = meta
                onComplete(meta)
            }
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
