import AppKit
import Foundation

enum SyntaxHighlighter {
    private static let tagRegex = CaptureTag.tagRegex

    private static let objectRegex: NSRegularExpression = {
        try! NSRegularExpression(pattern: #"(?<!\S)@([\p{L}\p{N}][\p{L}\p{N}._-]*)"#, options: [])
    }()

    private static let referenceRegex: NSRegularExpression = {
        try! NSRegularExpression(pattern: #"\[\[([^\]]+)\]\]"#, options: [])
    }()

    static func highlight(textStorage: NSTextStorage, editedRange: NSRange) {
        let fullRange = NSRange(location: 0, length: textStorage.length)
        let string = textStorage.string

        let defaultFont = NSFont.systemFont(ofSize: 14)
        textStorage.addAttribute(.font, value: defaultFont, range: fullRange)
        textStorage.addAttribute(.foregroundColor, value: NSColor.labelColor, range: fullRange)
        textStorage.removeAttribute(.backgroundColor, range: fullRange)
        textStorage.removeAttribute(.underlineStyle, range: fullRange)

        // #tag highlighting
        let tagMatches = tagRegex.matches(in: string, options: [], range: fullRange)
        for match in tagMatches {
            let tagRange = match.range
            guard let capturedRange = Range(match.range(at: 1), in: string) else { continue }
            let tagName = String(string[capturedRange]).lowercased()
            let kind = CaptureTag(rawValue: tagName)?.entryKind ?? .note
            let color = nsColor(for: kind)

            textStorage.addAttribute(.font, value: NSFont.boldSystemFont(ofSize: 14), range: tagRange)
            textStorage.addAttribute(.foregroundColor, value: color, range: tagRange)
            textStorage.addAttribute(.backgroundColor, value: color.withAlphaComponent(0.08), range: tagRange)
        }

        // @object highlighting
        let objectMatches = objectRegex.matches(in: string, options: [], range: fullRange)
        for match in objectMatches {
            let matchRange = match.range
            textStorage.addAttribute(.font, value: NSFont.systemFont(ofSize: 14, weight: .medium), range: matchRange)
            textStorage.addAttribute(.foregroundColor, value: NSColor.systemTeal, range: matchRange)
        }

        // [[reference]] highlighting
        let refMatches = referenceRegex.matches(in: string, options: [], range: fullRange)
        for match in refMatches {
            let matchRange = match.range
            textStorage.addAttribute(.foregroundColor, value: NSColor.controlAccentColor, range: matchRange)
            textStorage.addAttribute(.underlineStyle, value: NSUnderlineStyle.single.rawValue, range: matchRange)
        }
    }

    private static func nsColor(for kind: EntryKind) -> NSColor {
        switch kind {
        case .note: .secondaryLabelColor
        case .idea: .systemYellow
        case .question: .systemBlue
        case .claim: .systemOrange
        case .evidence: .systemGreen
        case .source: .systemPurple
        case .comparison: .systemCyan
        case .pattern: .systemIndigo
        case .plan: .systemTeal
        case .decided: .controlAccentColor
        case .solved: .systemGreen
        case .verified: .systemMint
        case .dropped: .systemRed
        case .handoff, .anchorWritten: .secondaryLabelColor
        }
    }
}
