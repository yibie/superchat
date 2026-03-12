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

func listItemType(for entry: Entry) -> ListItemType {
    entry.kind == .source ? .source : .entry
}

func listKindSymbol(for kind: ListKind) -> String {
    switch kind {
    case .topic: "number"
    case .queue: "line.3.horizontal.decrease.circle"
    case .pack: "shippingbox"
    case .collection: "square.stack"
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

// MARK: - EntryKindBadge (simplified: colored dot + text)

struct EntryKindBadge: View {
    let kind: EntryKind

    var body: some View {
        HStack(spacing: 4) {
            Circle()
                .fill(kind.kindColor)
                .frame(width: 6, height: 6)
            Text(kind.title)
                .font(.tnMicro.weight(.medium))
                .foregroundStyle(.secondary)
        }
    }
}

// MARK: - ListItemTypeBadge

struct ListItemTypeBadge: View {
    let type: ListItemType

    var body: some View {
        HStack(spacing: 4) {
            Circle()
                .fill(color)
                .frame(width: 6, height: 6)
            Text(title)
                .font(.tnMicro.weight(.medium))
                .foregroundStyle(.secondary)
        }
    }

    private var title: String {
        switch type {
        case .thread: "Thread"
        case .entry: "Note"
        case .source: "Source"
        }
    }

    private var color: Color {
        switch type {
        case .thread: .accentColor
        case .entry: .orange
        case .source: .purple
        }
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

// MARK: - EntryBodyView

struct EntryBodyView: View {
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            switch entry.body.kind {
            case .text:
                Text(entry.summaryText)
                    .font(.tnBody)
            case .url:
                if let urlString = entry.body.url, let url = URL(string: urlString) {
                    Link(destination: url) {
                        Label(entry.sourceMetadata?.title ?? url.host() ?? urlString, systemImage: "link")
                    }
                    .font(.tnBody)
                    if let locator = entry.sourceMetadata?.locator {
                        Text(locator)
                            .font(.tnCaption)
                            .foregroundStyle(.secondary)
                    }
                } else {
                    Text(entry.summaryText)
                        .font(.tnBody)
                }
            case .image:
                Label(entry.body.title ?? "Image", systemImage: "photo")
                    .font(.tnBody)
                if let details = entry.body.details {
                    Text(details)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }
            case .document:
                Label(entry.body.title ?? "Document", systemImage: "doc.richtext")
                    .font(.tnBody)
                if let details = entry.body.details {
                    Text(details)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }
            case .mixed:
                if let text = entry.body.text {
                    Text(text)
                        .font(.tnBody)
                }
                if let urlString = entry.body.url, let url = URL(string: urlString) {
                    Link(destination: url) {
                        Label(entry.sourceMetadata?.title ?? url.host() ?? urlString, systemImage: "link")
                    }
                    .font(.tnCaption)
                }
            }
        }
    }
}

// MARK: - AddToListMenu

struct AddToListMenu: View {
    @Environment(ThreadnoteStore.self) private var store
    let itemType: ListItemType
    let itemID: UUID

    var body: some View {
        if store.lists.isEmpty {
            Text("No lists available")
        } else {
            Text("Add to List")
            Divider()

            ForEach(store.lists.sorted { $0.updatedAt > $1.updatedAt }) { list in
                Button {
                    store.addToList(itemType: itemType, itemID: itemID, to: list.id)
                } label: {
                    Label(list.title, systemImage: isInList(list.id) ? "checkmark.circle.fill" : listKindSymbol(for: list.kind))
                }
            }
        }
    }

    private func isInList(_ listID: UUID) -> Bool {
        store.items(for: listID).contains { $0.itemType == itemType && $0.itemID == itemID }
    }
}

// MARK: - ListResolvedItem (shared type used by ListDocument & ResourceViews)

struct ListResolvedItem: Identifiable {
    let listItem: ListItem
    let thread: ThreadRecord?
    let entry: Entry?

    var id: UUID { listItem.id }
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
