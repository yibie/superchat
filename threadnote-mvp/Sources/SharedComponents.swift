import AppKit
import SwiftUI

// MARK: - Canvas layout (kept for legacy compatibility during transition)

let workspaceCanvasContentMaxWidth: CGFloat = 720

extension View {
    func workspaceCanvasColumn() -> some View {
        frame(maxWidth: workspaceCanvasContentMaxWidth, alignment: .leading)
            .frame(maxWidth: .infinity, alignment: .center)
    }
}

// MARK: - Free helpers (used across multiple view files)

func threadSecondarySummary(_ thread: ThreadRecord) -> String? {
    let title = thread.title.trimmingCharacters(in: .whitespacesAndNewlines)
    let goal = thread.goalLayer.goalStatement.trimmingCharacters(in: .whitespacesAndNewlines)
    guard !goal.isEmpty else { return nil }
    guard goal.compare(title, options: [.caseInsensitive, .diacriticInsensitive]) != .orderedSame else { return nil }
    return goal
}

func threadSubtitle(for thread: ThreadRecord) -> String {
    threadSecondarySummary(thread) ?? thread.goalLayer.goalType.title
}

// MARK: - Inline tag attributed text

@ViewBuilder
func entryTagBadge(kind: EntryKind) -> some View {
    EntryKindBadge(kind: kind)
}

func entryBodyText(text: String, font: Font) -> AttributedString {
    var body = AttributedString(text)
    body.font = font
    highlightInlineSyntax(in: &body, pattern: #"@([\p{L}\p{N}][\p{L}\p{N}._-]*)"#, color: .teal, font: font.weight(.medium))
    highlightInlineSyntax(
        in: &body,
        pattern: #"\[\[[^\]]+\]\]"#,
        color: .accentColor,
        font: font.weight(.medium),
        underlineStyle: .single
    )
    return body
}

func entryAttributedText(kind: EntryKind, text: String, font: Font) -> AttributedString {
    var tag = AttributedString("#\(kind.rawValue) ")
    tag.foregroundColor = kind.kindColor
    tag.font = font.weight(.medium)
    return tag + entryBodyText(text: text, font: font)
}

private func highlightInlineSyntax(
    in body: inout AttributedString,
    pattern: String,
    color: Color,
    font: Font,
    underlineStyle: Text.LineStyle? = nil
) {
    let text = String(body.characters)
    guard let regex = try? NSRegularExpression(pattern: pattern, options: []) else { return }

    let matches = regex.matches(
        in: text,
        options: [],
        range: NSRange(text.startIndex..<text.endIndex, in: text)
    )

    for match in matches.reversed() {
        guard let stringRange = Range(match.range, in: text),
              let attributedRange = Range(stringRange, in: body) else {
            continue
        }

        body[attributedRange].foregroundColor = color
        body[attributedRange].font = font
        if let underlineStyle {
            body[attributedRange].underlineStyle = underlineStyle
        }
    }
}

// MARK: - DocumentSection (replaces SectionCard)

struct DocumentSection<Content: View>: View {
    let title: String
    var systemImage: String? = nil
    @ViewBuilder let content: Content

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            HStack(spacing: TNSpacing.sm) {
                if let systemImage {
                    Image(systemName: systemImage)
                        .foregroundStyle(.secondary)
                }
                Text(title)
                    .font(.tnSectionTitle)
            }
            .padding(.bottom, TNSpacing.xs)

            content
        }
        .padding(.vertical, TNSpacing.md)
    }
}

// MARK: - SectionCard (kept for sheets that still use it)

struct SectionCard<Content: View>: View {
    let title: String
    let systemImage: String
    @ViewBuilder let content: Content

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Label(title, systemImage: systemImage)
                .font(.headline)
            content
        }
        .padding(18)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.lg))
        .overlay {
            RoundedRectangle(cornerRadius: TNCorner.lg)
                .stroke(Color.tnBorder, lineWidth: 1)
        }
    }
}

// MARK: - InlineExpandable

struct InlineExpandable<Content: View>: View {
    let title: String
    let count: Int
    var startExpanded: Bool = false
    @ViewBuilder let content: Content
    @State private var isExpanded: Bool

    init(title: String, count: Int, startExpanded: Bool = false, @ViewBuilder content: () -> Content) {
        self.title = title
        self.count = count
        self.startExpanded = startExpanded
        self.content = content()
        _isExpanded = State(initialValue: startExpanded)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            Button {
                withAnimation(.easeInOut(duration: 0.2)) {
                    isExpanded.toggle()
                }
            } label: {
                HStack(spacing: TNSpacing.sm) {
                    Image(systemName: isExpanded ? "chevron.down" : "chevron.right")
                        .font(.tnCaption)
                        .foregroundStyle(.tertiary)
                        .frame(width: 12)

                    Text(title)
                        .font(.tnSectionTitle)

                    Text("\(count)")
                        .font(.tnCaption)
                        .foregroundStyle(.tertiary)

                    Spacer()
                }
            }
            .buttonStyle(.plain)

            if isExpanded {
                content
                    .padding(.leading, TNSpacing.lg)
            }
        }
        .padding(.vertical, TNSpacing.sm)
    }
}

// MARK: - RelationBadge

struct RelationBadge: View {
    let kind: DiscourseRelationKind

    var body: some View {
        Label(kind.title, systemImage: symbol)
            .font(.tnMicro.weight(.medium))
            .foregroundStyle(.secondary)
    }

    private var symbol: String {
        switch kind {
        case .supports: "arrow.up.right.circle"
        case .opposes: "minus.circle"
        case .informs: "info.circle"
        case .answers: "checkmark.bubble"
        }
    }
}

struct EntryKindBadge: View {
    let kind: EntryKind

    var body: some View {
        HStack(spacing: 6) {
            Circle()
                .fill(kind.kindColor)
                .frame(width: 6, height: 6)
            Text(kind.title)
                .font(.tnMicro.weight(.medium))
                .foregroundStyle(kind.kindColor)
        }
        .padding(.horizontal, TNSpacing.sm)
        .padding(.vertical, 4)
        .background(kind.kindColor.opacity(0.10), in: Capsule())
        .overlay(
            Capsule()
                .stroke(kind.kindColor.opacity(0.18), lineWidth: 1)
        )
    }
}

// MARK: - Thin Divider

struct ThinDivider: View {
    var body: some View {
        Rectangle()
            .fill(Color.tnBorderSubtle)
            .frame(height: 1)
            .padding(.vertical, TNSpacing.xs)
    }
}
