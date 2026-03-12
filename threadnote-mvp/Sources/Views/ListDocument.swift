import SwiftUI

struct ListDocument: View {
    @Environment(ThreadnoteStore.self) private var store
    let list: ListRecord
    @State private var filter: ListDetailFilter = .all
    @State private var sortMode: ListSortMode = .saved

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Page title
            Text(list.title)
                .font(.tnPageTitle)

            if !list.description.isEmpty {
                Text(list.description)
                    .font(.tnBody)
                    .foregroundStyle(.secondary)
                    .padding(.top, TNSpacing.xs)
            }

            HStack(spacing: TNSpacing.md) {
                Text("\(list.kind.title) · \(resolvedItems.count) items")
                    .font(.tnCaption)
                    .foregroundStyle(.tertiary)

                Spacer()

                Picker("Filter", selection: $filter) {
                    ForEach(ListDetailFilter.allCases) { option in
                        Text(option.title).tag(option)
                    }
                }
                .pickerStyle(.segmented)
                .frame(maxWidth: 280)

                Menu {
                    Picker("Sort", selection: $sortMode) {
                        ForEach(ListSortMode.allCases) { option in
                            Text(option.title).tag(option)
                        }
                    }
                } label: {
                    Label(sortMode.title, systemImage: "arrow.up.arrow.down")
                        .font(.tnCaption)
                }
            }
            .padding(.vertical, TNSpacing.md)

            ThinDivider()

            // Content
            if filteredItems.isEmpty {
                ContentUnavailableView(
                    "No \(filter.title.lowercased()) here",
                    systemImage: "tray",
                    description: Text(filter == .all
                        ? "This list is empty. Add threads, notes, or sources from context menus."
                        : "Switch filters or add more resources.")
                )
                .frame(maxWidth: .infinity)
                .padding(.vertical, TNSpacing.xxl)
            } else {
                LazyVStack(alignment: .leading, spacing: 0) {
                    ForEach(filteredItems) { item in
                        ListDocumentRow(item: item)
                        ThinDivider()
                    }
                }
            }
        }
    }

    // MARK: - Data

    private var resolvedItems: [ListResolvedItem] {
        store.items(for: list.id).compactMap { item in
            switch item.itemType {
            case .thread:
                guard let thread = store.threads.first(where: { $0.id == item.itemID }) else { return nil }
                return ListResolvedItem(listItem: item, thread: thread, entry: nil)
            case .entry, .source:
                guard let entry = store.entries.first(where: { $0.id == item.itemID }) else { return nil }
                return ListResolvedItem(listItem: item, thread: nil, entry: entry)
            }
        }
    }

    private var filteredItems: [ListResolvedItem] {
        sortItems(resolvedItems.filter { filter.matches($0.listItem.itemType) })
    }

    private func sortItems(_ items: [ListResolvedItem]) -> [ListResolvedItem] {
        switch sortMode {
        case .saved:
            return items.sorted { lhs, rhs in
                if lhs.listItem.position == rhs.listItem.position {
                    return lhs.listItem.addedAt < rhs.listItem.addedAt
                }
                return lhs.listItem.position < rhs.listItem.position
            }
        case .recentAdded:
            return items.sorted { $0.listItem.addedAt > $1.listItem.addedAt }
        case .recentActive:
            return items.sorted { sortDate(for: $0) > sortDate(for: $1) }
        }
    }

    private func sortDate(for item: ListResolvedItem) -> Date {
        if let thread = item.thread { return thread.lastActiveAt }
        if let entry = item.entry, let thread = store.thread(for: entry) {
            return max(entry.createdAt, thread.lastActiveAt)
        }
        return item.listItem.addedAt
    }
}

// MARK: - ListDocumentRow (flat, compact)

struct ListDocumentRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let item: ListResolvedItem

    var body: some View {
        HStack(alignment: .top, spacing: TNSpacing.md) {
            ListItemTypeBadge(type: item.listItem.itemType)

            VStack(alignment: .leading, spacing: TNSpacing.xs) {
                if let thread = item.thread {
                    Text(thread.title)
                        .font(.tnBody.weight(.medium))
                    if let subtitle = threadSecondarySummary(thread) {
                        Text(subtitle)
                            .font(.tnCaption)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                } else if let entry = item.entry {
                    if entry.isSourceResource {
                        Text(entry.sourceDisplayTitle)
                            .font(.tnBody.weight(.medium))
                            .lineLimit(2)
                    } else {
                        HStack(spacing: TNSpacing.sm) {
                            EntryKindBadge(kind: entry.kind)
                        }
                        Text(entry.summaryText)
                            .font(.tnBody)
                            .lineLimit(2)
                    }
                }
            }

            Spacer()

            VStack(alignment: .trailing, spacing: TNSpacing.xs) {
                if item.listItem.isPinned {
                    Image(systemName: "pin.fill")
                        .font(.tnMicro)
                        .foregroundStyle(Color.accentColor)
                }
                Text(item.listItem.addedAt, format: .dateTime.month(.abbreviated).day())
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }
        }
        .padding(.vertical, TNSpacing.sm)
        .contentShape(Rectangle())
        .onTapGesture {
            if let thread = item.thread {
                store.openThread(thread.id)
            } else if let entry = item.entry, entry.isSourceResource {
                store.openSource(entry.id)
            }
        }
        .contextMenu {
            Button(item.listItem.isPinned ? "Unpin" : "Pin", systemImage: item.listItem.isPinned ? "pin.slash" : "pin") {
                store.togglePinned(item.listItem.id)
            }
            Button("Remove from List", systemImage: "minus.circle") {
                store.removeFromList(item.listItem.id)
            }
        }
    }
}

// MARK: - Filter & Sort (private enums reused from ListViews)

private enum ListDetailFilter: String, CaseIterable, Identifiable {
    case all, threads, notes, sources
    var id: Self { self }
    var title: String {
        switch self {
        case .all: "All"
        case .threads: "Threads"
        case .notes: "Notes"
        case .sources: "Sources"
        }
    }
    func matches(_ type: ListItemType) -> Bool {
        switch self {
        case .all: true
        case .threads: type == .thread
        case .notes: type == .entry
        case .sources: type == .source
        }
    }
}

private enum ListSortMode: String, CaseIterable, Identifiable {
    case saved, recentAdded, recentActive
    var id: Self { self }
    var title: String {
        switch self {
        case .saved: "Saved"
        case .recentAdded: "Recent"
        case .recentActive: "Active"
        }
    }
}
