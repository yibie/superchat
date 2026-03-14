import Foundation

enum URLBodyMigration {
    static let metadataKey = "urlBodyMigrationV1"

    static func runIfNeeded(entries: inout [Entry]) -> [Entry] {
        var modified: [Entry] = []

        for index in entries.indices {
            guard entries[index].body.kind == .text,
                  let migrated = bodyAndMetadataIfNeeded(
                      from: entries[index].summaryText,
                      sourceMetadata: entries[index].sourceMetadata
                  ) else { continue }

            entries[index].body = migrated.body
            entries[index].sourceMetadata = migrated.sourceMetadata
            modified.append(entries[index])
        }

        return modified
    }

    static func bodyAndMetadataIfNeeded(
        from rawText: String,
        sourceMetadata: SourceMetadata?
    ) -> (body: EntryBody, sourceMetadata: SourceMetadata?)? {
        let summary = normalizedSummaryText(from: rawText)
        if let attachmentBody = attachmentBodyIfNeeded(from: summary) {
            return (
                attachmentBody,
                Self.sourceMetadata(for: attachmentBody, sourceMetadata: sourceMetadata)
            )
        }

        guard !summary.isEmpty,
              let detector = linkDetector else { return nil }

        let range = NSRange(summary.startIndex..<summary.endIndex, in: summary)
        let matches = detector.matches(in: summary, options: [], range: range)
        guard let firstURL = matches.first?.url?.absoluteString else { return nil }

        let isOnlyURL = summary == firstURL
        var metadata = sourceMetadata
        let locator = metadata?.locator?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        if metadata == nil {
            metadata = SourceMetadata(title: nil, locator: firstURL, citation: nil)
        } else if locator.isEmpty {
            metadata?.locator = firstURL
        }

        return (
            EntryBody(
                kind: isOnlyURL ? .url : .mixed,
                text: isOnlyURL ? nil : summary,
                url: firstURL,
                title: nil,
                details: nil,
                linkMeta: nil
            ),
            metadata
        )
    }

    static func attachmentBodyIfNeeded(from text: String) -> EntryBody? {
        let summary = normalizedSummaryText(from: text)
        guard !summary.isEmpty,
              let detector = attachmentDetector,
              let match = detector.firstMatch(
                  in: summary,
                  options: [],
                  range: NSRange(summary.startIndex..<summary.endIndex, in: summary)
              ),
              let pathRange = Range(match.range(at: 1), in: summary) else {
            return nil
        }

        let path = String(summary[pathRange]).trimmingCharacters(in: .whitespacesAndNewlines)
        guard path.lowercased().hasPrefix("attachments/") else { return nil }

        let contentType = attachmentContentType(for: path)
        let displayName = URL(fileURLWithPath: path).lastPathComponent
        let remainingText = attachmentTextAfterRemovingPath(summary, range: match.range(at: 1))
        let title = displayName.isEmpty ? nil : displayName
        let linkMeta = LinkMetadata(contentType: contentType)

        if let remainingText, !remainingText.isEmpty {
            return EntryBody(
                kind: .mixed,
                text: remainingText,
                url: path,
                title: title,
                details: nil,
                linkMeta: linkMeta
            )
        }

        switch contentType {
        case .image:
            return EntryBody(kind: .image, text: nil, url: path, title: title, details: nil, linkMeta: linkMeta)
        case .video, .audio:
            return EntryBody(kind: .url, text: nil, url: path, title: title, details: nil, linkMeta: linkMeta)
        case .document, .webpage:
            return EntryBody(kind: .document, text: nil, url: path, title: title, details: nil, linkMeta: linkMeta)
        }
    }

    static func attachmentContentType(for path: String) -> LinkContentType {
        switch URL(fileURLWithPath: path).pathExtension.lowercased() {
        case "png", "jpg", "jpeg", "gif", "webp", "heic", "tiff", "bmp":
            return .image
        case "mp4", "mov", "m4v", "avi", "mkv":
            return .video
        case "mp3", "m4a", "wav", "aac", "ogg", "flac":
            return .audio
        default:
            return .document
        }
    }

    static func sourceMetadata(
        for body: EntryBody,
        sourceMetadata: SourceMetadata?
    ) -> SourceMetadata? {
        guard let locator = body.url, !locator.isEmpty else { return sourceMetadata }

        var metadata = sourceMetadata
        let existingLocator = metadata?.locator?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        let existingTitle = metadata?.title?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""

        if metadata == nil {
            metadata = SourceMetadata(title: body.title, locator: locator, citation: nil)
        } else {
            if existingLocator.isEmpty {
                metadata?.locator = locator
            }
            if existingTitle.isEmpty {
                metadata?.title = body.title
            }
        }

        return metadata
    }

    static func normalizedSummaryText(from rawText: String) -> String {
        rawText
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .replacingOccurrences(of: "\n", with: " ")
    }

    private static func attachmentTextAfterRemovingPath(_ summary: String, range: NSRange) -> String? {
        guard let pathRange = Range(range, in: summary) else { return nil }

        let prefix = summary[..<pathRange.lowerBound]
        let suffix = summary[pathRange.upperBound...]
        let combined = "\(prefix) \(suffix)"
            .replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression)
            .trimmingCharacters(in: .whitespacesAndNewlines)

        return combined.isEmpty ? nil : combined
    }

    private static let linkDetector = try? NSDataDetector(
        types: NSTextCheckingResult.CheckingType.link.rawValue
    )

    private static let attachmentDetector = try? NSRegularExpression(
        pattern: #"(?<!\S)(attachments/[^\s]+)"#,
        options: [.caseInsensitive]
    )
}
