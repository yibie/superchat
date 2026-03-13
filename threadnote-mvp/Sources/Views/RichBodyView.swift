import AppKit
import SwiftUI

// MARK: - RichBodyView (dispatcher by EntryBodyKind)

struct RichBodyView: View {
    let entry: Entry

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
        if let urlString = entry.body.url, let url = URL(string: urlString) {
            if let meta = entry.body.linkMeta {
                switch meta.contentType {
                case .video:
                    VideoCardView(url: url, metadata: meta, title: meta.metaTitle ?? entry.sourceMetadata?.title)
                case .image:
                    ImagePreviewView(urlString: meta.imageURL ?? urlString)
                default:
                    LinkCardView(url: url, metadata: meta, fallbackTitle: entry.sourceMetadata?.title)
                }
            } else {
                MinimalLinkRow(url: url, title: entry.sourceMetadata?.title)
            }
        }
    }

    @ViewBuilder
    private var imageBodyView: some View {
        if let urlString = entry.body.url {
            ImagePreviewView(urlString: urlString)
        } else {
            Label(entry.body.title ?? "Image", systemImage: "photo")
                .font(.tnBody)
        }
    }

    @ViewBuilder
    private var documentBodyView: some View {
        Label(entry.body.title ?? "Document", systemImage: "doc.richtext")
            .font(.tnBody)
        if let details = entry.body.details {
            Text(details)
                .font(.tnCaption)
                .foregroundStyle(.secondary)
        }
    }

    @ViewBuilder
    private var mixedBodyView: some View {
        if let urlString = entry.body.url, let url = URL(string: urlString) {
            if let meta = entry.body.linkMeta {
                switch meta.contentType {
                case .video:
                    VideoCardView(url: url, metadata: meta, title: meta.metaTitle ?? entry.sourceMetadata?.title)
                default:
                    LinkCardView(url: url, metadata: meta, fallbackTitle: entry.sourceMetadata?.title)
                }
            } else {
                MinimalLinkRow(url: url, title: entry.sourceMetadata?.title)
            }
        }
    }
}

// MARK: - LinkCardView

struct LinkCardView: View {
    let url: URL
    let metadata: LinkMetadata
    var fallbackTitle: String?

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
                            image.resizable().aspectRatio(contentMode: .fill)
                        default:
                            Color.tnBackground
                        }
                    }
                    .frame(width: 48, height: 48)
                    .clipShape(.rect(cornerRadius: TNCorner.sm))
                }

                VStack(alignment: .leading, spacing: 2) {
                    Text(displayTitle)
                        .font(.tnCaption.weight(.medium))
                        .lineLimit(1)
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
            .background(Color.tnBackground, in: .rect(cornerRadius: TNCorner.sm))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.sm)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }
}

// MARK: - VideoCardView

struct VideoCardView: View {
    let url: URL
    let metadata: LinkMetadata
    var title: String?

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            VStack(alignment: .leading, spacing: TNSpacing.xs) {
                if let imageURL = metadata.imageURL, let imgURL = URL(string: imageURL) {
                    ZStack {
                        AsyncImage(url: imgURL) { phase in
                            switch phase {
                            case .success(let image):
                                image.resizable().aspectRatio(contentMode: .fill)
                            default:
                                Color.tnBackground
                            }
                        }
                        .frame(maxHeight: 180)
                        .clipShape(.rect(cornerRadius: TNCorner.sm))

                        Image(systemName: "play.circle.fill")
                            .font(.system(size: 44))
                            .foregroundStyle(.white)
                            .shadow(color: .black.opacity(0.3), radius: 4, y: 2)
                    }
                }

                HStack(spacing: TNSpacing.xs) {
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
                    Spacer(minLength: 0)
                }
            }
            .padding(TNSpacing.sm)
            .background(Color.tnBackground, in: .rect(cornerRadius: TNCorner.sm))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.sm)
                    .stroke(Color.tnBorderSubtle, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }
}

// MARK: - ImagePreviewView

struct ImagePreviewView: View {
    let urlString: String

    var body: some View {
        if let url = URL(string: urlString) {
            AsyncImage(url: url) { phase in
                switch phase {
                case .success(let image):
                    image.resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(maxHeight: 200)
                        .clipShape(.rect(cornerRadius: TNCorner.sm))
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
    }
}

// MARK: - MinimalLinkRow (loading fallback)

struct MinimalLinkRow: View {
    let url: URL
    var title: String?

    var body: some View {
        Button {
            NSWorkspace.shared.open(url)
        } label: {
            Label(title ?? url.host() ?? url.absoluteString, systemImage: "link")
                .font(.tnBody)
                .foregroundStyle(.secondary)
        }
        .buttonStyle(.plain)
    }
}
