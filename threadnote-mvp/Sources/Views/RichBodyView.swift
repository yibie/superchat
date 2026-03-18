import AppKit
import AVFoundation
import QuickLookUI
import SwiftUI

// MARK: - RichBodyView (dispatcher by EntryBodyKind)

struct RichBodyView: View {
    @Environment(WorkspaceManager.self) private var workspace
    let entry: Entry

    private var previewTitle: String? {
        entry.body.title ?? entry.sourceMetadata?.title
    }

    var body: some View {
        switch entry.body.kind {
        case .text:
            EmptyView()
        case .url:
            urlBodyView
        case .image:
            imageBodyView
        case .document:
            documentBodyView
        case .mixed:
            mixedBodyView
        }
    }

    @ViewBuilder
    private var urlBodyView: some View {
        if let urlString = entry.body.url {
            bodyPreview(
                for: urlString,
                contentType: entry.body.linkMeta?.contentType,
                metadata: entry.body.linkMeta,
                title: previewTitle
            )
        }
    }

    @ViewBuilder
    private var imageBodyView: some View {
        if let urlString = entry.body.url {
            bodyPreview(
                for: urlString,
                contentType: .image,
                metadata: entry.body.linkMeta,
                title: previewTitle
            )
        } else {
            Label(entry.body.title ?? "Image", systemImage: "photo")
                .font(.tnBody)
        }
    }

    @ViewBuilder
    private var documentBodyView: some View {
        if let urlString = entry.body.url {
            bodyPreview(
                for: urlString,
                contentType: .document,
                metadata: entry.body.linkMeta,
                title: previewTitle
            )
        } else {
            Label(entry.body.title ?? "Document", systemImage: "doc.richtext")
                .font(.tnBody)
        }
    }

    @ViewBuilder
    private var mixedBodyView: some View {
        if let urlString = entry.body.url {
            bodyPreview(
                for: urlString,
                contentType: entry.body.linkMeta?.contentType,
                metadata: entry.body.linkMeta,
                title: previewTitle
            )
        }
    }

    @ViewBuilder
    private func bodyPreview(
        for urlString: String,
        contentType: LinkContentType?,
        metadata: LinkMetadata?,
        title: String?
    ) -> some View {
        let resolved = resolvedURL(for: urlString)
        let effectiveType = contentType ?? inferredContentType(for: urlString)

        if isLocalPath(urlString) {
            if let resolved, resolved.isFileURL {
                localPreview(for: resolved, contentType: effectiveType, title: title)
            } else {
                MissingLocalFileView(title: title ?? missingFileDisplayName(for: urlString))
            }
        } else if let resolved, resolved.isFileURL {
            localPreview(for: resolved, contentType: effectiveType, title: title)
        } else if let remoteURL = URL(string: urlString) {
            remotePreview(for: remoteURL, urlString: urlString, contentType: effectiveType, metadata: metadata, title: title)
        }
    }

    @ViewBuilder
    private func localPreview(for url: URL, contentType: LinkContentType, title: String?) -> some View {
        switch contentType {
        case .image:
            LocalImageView(url: url)
        case .video:
            LocalVideoView(url: url, title: title)
        case .audio:
            AudioPlayerView(url: url, title: title)
        case .document, .webpage:
            LocalDocumentCardView(url: url, title: title)
        }
    }

    @ViewBuilder
    private func remotePreview(
        for url: URL,
        urlString: String,
        contentType: LinkContentType,
        metadata: LinkMetadata?,
        title: String?
    ) -> some View {
        switch contentType {
        case .image:
            ImagePreviewView(url: url)
        case .video:
            VideoCardView(url: url, metadata: metadata ?? LinkMetadata(contentType: .video), title: title)
        case .audio:
            MinimalLinkRow(url: url, title: title ?? url.absoluteString, systemImage: "waveform")
        case .document:
            RemoteDocumentCardView(url: url, title: title ?? URL(fileURLWithPath: urlString).lastPathComponent)
        case .webpage:
            if let metadata {
                LinkCardView(url: url, metadata: metadata, fallbackTitle: title)
            } else {
                MinimalLinkRow(url: url, title: title)
            }
        }
    }

    private func resolvedURL(for path: String) -> URL? {
        let trimmed = path.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return nil }

        if trimmed.hasPrefix("attachments/") {
            guard let workspaceURL = workspace.workspaceURL else { return nil }
            let localURL = workspaceURL.appending(path: trimmed)
            guard FileManager.default.fileExists(atPath: localURL.path) else { return nil }
            return localURL
        }

        if trimmed.hasPrefix("/") {
            let localURL = URL(fileURLWithPath: trimmed)
            guard FileManager.default.fileExists(atPath: localURL.path) else { return nil }
            return localURL
        }

        if let url = URL(string: trimmed), url.isFileURL {
            guard FileManager.default.fileExists(atPath: url.path) else { return nil }
            return url
        }

        if let url = URL(string: trimmed), url.scheme != nil {
            return url
        }

        return nil
    }

    private func isLocalPath(_ urlString: String) -> Bool {
        let trimmed = urlString.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmed.hasPrefix("attachments/") || trimmed.hasPrefix("/") {
            return true
        }
        if let url = URL(string: trimmed), url.isFileURL {
            return true
        }
        return false
    }

    private func missingFileDisplayName(for urlString: String) -> String {
        let trimmed = urlString.trimmingCharacters(in: .whitespacesAndNewlines)
        if let url = URL(string: trimmed), url.isFileURL {
            return url.lastPathComponent
        }
        return URL(fileURLWithPath: trimmed).lastPathComponent
    }

    private func inferredContentType(for urlString: String) -> LinkContentType {
        let trimmed = urlString.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmed.hasPrefix("attachments/") || trimmed.hasPrefix("/") {
            return URLBodyMigration.attachmentContentType(for: trimmed)
        }
        if let url = URL(string: trimmed), let scheme = url.scheme, !scheme.isEmpty {
            let ext = url.pathExtension.lowercased()
            switch ext {
            case "png", "jpg", "jpeg", "gif", "webp", "heic", "tiff", "bmp":
                return .image
            case "mp4", "mov", "m4v", "avi", "mkv":
                return .video
            case "mp3", "m4a", "wav", "aac", "ogg", "flac":
                return .audio
            case "", "html", "htm":
                return .webpage
            default:
                return .document
            }
        }
        return .document
    }
}

// MARK: - LinkCardView

struct LinkCardView: View {
    let url: URL
    let metadata: LinkMetadata
    var fallbackTitle: String?

    @State private var isHovered = false

    private var displayTitle: String {
        metadata.metaTitle ?? fallbackTitle ?? url.host() ?? url.absoluteString
    }

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            HStack(spacing: TNSpacing.sm) {
                if let imageURL = metadata.imageURL, let imgURL = URL(string: imageURL) {
                    AsyncImage(url: imgURL) { phase in
                        switch phase {
                        case .success(let image):
                            image
                                .resizable()
                                .aspectRatio(contentMode: .fill)
                        default:
                            Color.tnBackground
                        }
                    }
                    .frame(width: 56, height: 56)
                    .clipShape(.rect(cornerRadius: TNCorner.card))
                }

                VStack(alignment: .leading, spacing: 2) {
                    Text(displayTitle)
                        .font(.tnCaption.weight(.medium))
                        .lineLimit(2)
                        .foregroundStyle(.primary)

                    Text(metadata.siteName ?? url.host() ?? "")
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                        .lineLimit(1)

                    if let desc = metadata.metaDescription, !desc.isEmpty {
                        Text(desc)
                            .font(.tnMicro)
                            .foregroundStyle(.secondary)
                            .lineLimit(2)
                    }
                }

                Spacer(minLength: 0)
            }
            .padding(TNSpacing.sm)
            .background(
                (isHovered ? Color.tnBackground : Color.tnSurface),
                in: .rect(cornerRadius: TNCorner.card)
            )
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.card)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
        .onHover { isHovered = $0 }
    }
}

// MARK: - VideoCardView

struct VideoCardView: View {
    let url: URL
    let metadata: LinkMetadata
    var title: String?

    @State private var isHovered = false

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            VStack(alignment: .leading, spacing: TNSpacing.xs) {
                ZStack {
                    if let imageURL = metadata.imageURL, let imgURL = URL(string: imageURL) {
                        AsyncImage(url: imgURL) { phase in
                            switch phase {
                            case .success(let image):
                                image
                                    .resizable()
                                    .aspectRatio(contentMode: .fill)
                            default:
                                Color.tnBackground
                            }
                        }
                    } else {
                        Color.tnBackground
                    }
                }
                .frame(maxWidth: .infinity)
                .frame(maxHeight: 240)
                .aspectRatio(16 / 9, contentMode: .fit)
                .clipShape(.rect(cornerRadius: TNCorner.card))
                .overlay(alignment: .center) {
                    Image(systemName: "play.circle.fill")
                        .font(.system(size: 44))
                        .foregroundStyle(.white)
                        .shadow(color: .black.opacity(0.3), radius: 4, y: 2)
                }

                VStack(alignment: .leading, spacing: 2) {
                    if let title = title ?? metadata.metaTitle {
                        Text(title)
                            .font(.tnCaption.weight(.medium))
                            .lineLimit(2)
                            .foregroundStyle(.primary)
                    }
                    Text(metadata.siteName ?? url.host() ?? "")
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                }
            }
            .padding(TNSpacing.sm)
            .background(
                (isHovered ? Color.tnBackground : Color.tnSurface),
                in: .rect(cornerRadius: TNCorner.card)
            )
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.card)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
        .onHover { isHovered = $0 }
    }
}

// MARK: - ImagePreviewView

struct ImagePreviewView: View {
    let url: URL

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            AsyncImage(url: url) { phase in
                switch phase {
                case .success(let image):
                    image
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(maxHeight: 280)
                        .clipShape(.rect(cornerRadius: TNCorner.card))
                case .failure:
                    Label("Image unavailable", systemImage: "photo")
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                default:
                    ProgressView()
                        .frame(height: 80)
                }
            }
        }
        .buttonStyle(.plain)
    }
}

// MARK: - Local media/document views

private struct PreviewItem: Identifiable {
    let id = UUID()
    let url: URL
}

struct LocalImageView: View {
    let url: URL
    @State private var previewItem: PreviewItem?

    var body: some View {
        Button {
            previewItem = PreviewItem(url: url)
        } label: {
            if let image = NSImage(contentsOf: url) {
                Image(nsImage: image)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxHeight: 280)
                    .clipShape(.rect(cornerRadius: TNCorner.card))
            } else {
                Label("Image unavailable", systemImage: "photo")
                    .font(.tnCaption)
                    .foregroundStyle(.secondary)
            }
        }
        .buttonStyle(.plain)
        .sheet(item: $previewItem) { item in
            QuickLookPreviewSheet(url: item.url)
                .frame(minWidth: 720, minHeight: 520)
        }
    }
}

struct AudioPlayerView: View {
    let url: URL
    var title: String?

    @StateObject private var controller: AudioPlaybackController

    init(url: URL, title: String?) {
        self.url = url
        self.title = title
        _controller = StateObject(wrappedValue: AudioPlaybackController(url: url))
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            HStack(spacing: TNSpacing.sm) {
                Button {
                    controller.togglePlayback()
                } label: {
                    Image(systemName: controller.isPlaying ? "pause.fill" : "play.fill")
                        .frame(width: 28, height: 28)
                }
                .buttonStyle(.borderless)

                VStack(alignment: .leading, spacing: 2) {
                    Text(title ?? url.lastPathComponent)
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.primary)
                        .lineLimit(1)
                    Text(controller.statusText)
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                }

                Spacer(minLength: 0)

                Text(controller.timeLabel)
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
                    .monospacedDigit()
            }

            Slider(
                value: Binding(
                    get: { controller.progress },
                    set: { controller.seek(to: $0) }
                ),
                in: 0...1
            )
            .disabled(!controller.isReady)
        }
        .padding(TNSpacing.sm)
        .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.card))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.card)
                .stroke(Color.tnBorderSubtle, lineWidth: 1)
        )
    }
}

struct LocalVideoView: View {
    let url: URL
    var title: String?

    @State private var thumbnail: NSImage?

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            VStack(alignment: .leading, spacing: TNSpacing.xs) {
                ZStack {
                    if let thumbnail {
                        Image(nsImage: thumbnail)
                            .resizable()
                            .aspectRatio(contentMode: .fill)
                    } else {
                        Color.tnBackground
                    }
                }
                .frame(maxWidth: .infinity)
                .frame(maxHeight: 240)
                .aspectRatio(16 / 9, contentMode: .fit)
                .clipShape(.rect(cornerRadius: TNCorner.card))
                .overlay(alignment: .center) {
                    Image(systemName: "play.circle.fill")
                        .font(.system(size: 44))
                        .foregroundStyle(.white)
                        .shadow(color: .black.opacity(0.3), radius: 4, y: 2)
                }

                HStack(spacing: TNSpacing.sm) {
                    VStack(alignment: .leading, spacing: 2) {
                        Text(title ?? url.lastPathComponent)
                            .font(.tnCaption.weight(.medium))
                            .foregroundStyle(.primary)
                            .lineLimit(2)
                        Text(fileDetailLabel(for: url))
                            .font(.tnMicro)
                            .foregroundStyle(.tertiary)
                    }
                    Spacer(minLength: 0)
                }
            }
            .padding(TNSpacing.sm)
            .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.card))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.card)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
        .task(id: url) {
            loadThumbnailIfNeeded()
        }
    }

    private func loadThumbnailIfNeeded() {
        guard thumbnail == nil else { return }
        let asset = AVAsset(url: url)
        let generator = AVAssetImageGenerator(asset: asset)
        generator.appliesPreferredTrackTransform = true
        if let image = try? generator.copyCGImage(at: .init(seconds: 0, preferredTimescale: 600), actualTime: nil) {
            thumbnail = NSImage(cgImage: image, size: .zero)
        }
    }
}

struct LocalDocumentCardView: View {
    let url: URL
    var title: String?

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            HStack(spacing: TNSpacing.sm) {
                Image(nsImage: NSWorkspace.shared.icon(forFile: url.path))
                    .resizable()
                    .frame(width: 36, height: 36)

                VStack(alignment: .leading, spacing: 2) {
                    Text(title ?? url.lastPathComponent)
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.primary)
                        .lineLimit(2)
                    Text(fileDetailLabel(for: url))
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                }

                Spacer(minLength: 0)
            }
            .padding(TNSpacing.sm)
            .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.card))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.card)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }
}

struct RemoteDocumentCardView: View {
    let url: URL
    var title: String

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            HStack(spacing: TNSpacing.sm) {
                Image(systemName: "doc")
                    .font(.system(size: 24))
                    .foregroundStyle(.secondary)
                    .frame(width: 36, height: 36)

                VStack(alignment: .leading, spacing: 2) {
                    Text(title)
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.primary)
                        .lineLimit(2)
                    Text(url.host() ?? url.absoluteString)
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                        .lineLimit(1)
                }

                Spacer(minLength: 0)
            }
            .padding(TNSpacing.sm)
            .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.card))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.card)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }
}

struct MissingLocalFileView: View {
    var title: String

    var body: some View {
        HStack(spacing: TNSpacing.sm) {
            Image(systemName: "exclamationmark.triangle")
                .font(.system(size: 20))
                .foregroundStyle(.orange)
                .frame(width: 36, height: 36)

            VStack(alignment: .leading, spacing: 2) {
                Text(title.isEmpty ? "本地文件" : title)
                    .font(.tnCaption.weight(.medium))
                    .foregroundStyle(.primary)
                    .lineLimit(2)
                Text("文件不可用")
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }

            Spacer(minLength: 0)
        }
        .padding(TNSpacing.sm)
        .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.card))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.card)
                .stroke(Color.tnBorderSubtle, lineWidth: 1)
        )
    }
}

// MARK: - MinimalLinkRow (loading fallback)

struct MinimalLinkRow: View {
    let url: URL
    var title: String?
    var systemImage: String = "link"

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            Label(title ?? url.host() ?? url.absoluteString, systemImage: systemImage)
                .font(.tnBody)
                .foregroundStyle(.secondary)
        }
        .buttonStyle(.plain)
    }
}

// MARK: - Quick Look

struct QuickLookPreviewSheet: NSViewRepresentable {
    let url: URL

    func makeNSView(context: Context) -> NSView {
        let container = NSView()
        if let preview = QLPreviewView(frame: .zero, style: .normal) {
            preview.frame = container.bounds
            preview.autoresizingMask = [.width, .height]
            preview.previewItem = url as NSURL
            preview.autostarts = true
            container.addSubview(preview)
        }
        return container
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        (nsView.subviews.first as? QLPreviewView)?.previewItem = url as NSURL
    }
}

// MARK: - Audio playback

final class AudioPlaybackController: NSObject, ObservableObject, AVAudioPlayerDelegate {
    @Published private(set) var isPlaying = false
    @Published private(set) var progress: Double = 0
    @Published private(set) var isReady = false

    private var player: AVAudioPlayer?
    private var timer: Timer?

    let url: URL

    init(url: URL) {
        self.url = url
        super.init()
        preparePlayer()
    }

    var statusText: String {
        guard isReady else { return "Audio unavailable" }
        return isPlaying ? "Playing" : "Ready"
    }

    var timeLabel: String {
        guard let player else { return "0:00 / 0:00" }
        return "\(formatDuration(player.currentTime)) / \(formatDuration(player.duration))"
    }

    func togglePlayback() {
        guard let player else { return }
        if player.isPlaying {
            player.pause()
            stopTimer()
            isPlaying = false
        } else {
            player.play()
            startTimer()
            isPlaying = true
        }
    }

    func seek(to newValue: Double) {
        guard let player else { return }
        progress = max(0, min(1, newValue))
        player.currentTime = player.duration * progress
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        stopTimer()
        isPlaying = false
        progress = 1
    }

    deinit {
        stopTimer()
    }

    private func preparePlayer() {
        guard let player = try? AVAudioPlayer(contentsOf: url) else { return }
        player.delegate = self
        player.prepareToPlay()
        self.player = player
        isReady = true
    }

    private func startTimer() {
        stopTimer()
        timer = Timer.scheduledTimer(
            timeInterval: 0.2,
            target: self,
            selector: #selector(handleTimerTick),
            userInfo: nil,
            repeats: true
        )
    }

    private func stopTimer() {
        timer?.invalidate()
        timer = nil
    }

    @objc
    private func handleTimerTick() {
        guard let player else { return }
        if player.duration > 0 {
            progress = player.currentTime / player.duration
        }
    }
}

// MARK: - Helpers

private func fileDetailLabel(for url: URL) -> String {
    let size = fileSizeString(for: url)
    return size.isEmpty ? "Local file" : size
}

private func fileSizeString(for url: URL) -> String {
    guard let attributes = try? FileManager.default.attributesOfItem(atPath: url.path),
          let size = attributes[.size] as? NSNumber else {
        return ""
    }

    let formatter = ByteCountFormatter()
    formatter.countStyle = .file
    return formatter.string(fromByteCount: size.int64Value)
}

private func formatDuration(_ duration: TimeInterval) -> String {
    guard duration.isFinite, duration >= 0 else { return "0:00" }
    let totalSeconds = Int(duration.rounded(.down))
    let minutes = totalSeconds / 60
    let seconds = totalSeconds % 60
    return String(format: "%d:%02d", minutes, seconds)
}
