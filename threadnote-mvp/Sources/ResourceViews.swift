import AppKit
import SwiftUI

// MARK: - ResourceOverviewRow

struct ResourceOverviewRow: View {
    let items: [ListResolvedItem]

    private var counts: (threads: Int, notes: Int, sources: Int) {
        var t = 0, n = 0, s = 0
        for item in items {
            switch item.listItem.itemType {
            case .thread: t += 1
            case .entry:  n += 1
            case .source: s += 1
            }
        }
        return (t, n, s)
    }

    var body: some View {
        let c = counts
        HStack(spacing: TNSpacing.sm) {
            Label("\(c.threads) threads", systemImage: "square.stack.3d.up")
            Divider()
            Label("\(c.notes) notes", systemImage: "note.text")
            Divider()
            Label("\(c.sources) sources", systemImage: "bookmark")
            Spacer(minLength: 0)
        }
        .font(.tnCaption)
        .foregroundStyle(.secondary)
        .padding(.horizontal, TNSpacing.md)
        .padding(.vertical, TNSpacing.sm)
        .background(Color.tnSurface.opacity(0.5), in: .rect(cornerRadius: TNCorner.sm))
    }
}

// MARK: - SourceDetailSheet

struct SourceDetailSheet: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.lg) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: TNSpacing.sm) {
                    Label("Source", systemImage: "bookmark")
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.secondary)
                    Text(entry.sourceDisplayTitle)
                        .font(.tnPageTitle)
                }

                Spacer()

                Button("Done") {
                    store.closeSource()
                }
                .buttonStyle(.bordered)
            }

            if let locator = entry.sourceLocator {
                DocumentSection(title: "Locator", systemImage: "link") {
                    Text(locator)
                        .textSelection(.enabled)
                        .font(.tnBody)

                    if let url = URL(string: locator), let scheme = url.scheme {
                        Button(scheme.hasPrefix("http") ? "Open in Browser" : "Open Resource", systemImage: "arrow.up.right.square") {
                            NSWorkspace.shared.open(url)
                        }
                        .buttonStyle(.borderedProminent)
                        .controlSize(.small)
                    }
                }
            }

            DocumentSection(title: "Summary", systemImage: "text.justify") {
                EntryBodyView(entry: entry)
                if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                    ThinDivider()
                    Text(citation)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }
            }

            if let thread = store.thread(for: entry) {
                DocumentSection(title: "Problem Space", systemImage: "square.stack.3d.up") {
                    VStack(alignment: .leading, spacing: TNSpacing.sm) {
                        Text(thread.title)
                            .font(.tnSectionTitle)
                        if let subtitle = threadSecondarySummary(thread) {
                            Text(subtitle)
                                .font(.tnBody)
                                .foregroundStyle(.secondary)
                        }
                        Button("Open Thread") {
                            store.openThread(thread.id)
                            store.closeSource()
                        }
                        .buttonStyle(.bordered)
                        .controlSize(.small)
                    }
                }
            }

            HStack {
                Spacer()
                Menu {
                    AddToListMenu(itemType: .source, itemID: entry.id)
                } label: {
                    Label("Add to List", systemImage: "plus.rectangle.on.folder")
                }
            }

            Spacer(minLength: 0)
        }
        .padding(TNSpacing.lg)
        .background(Color.tnBackground)
    }
}

// MARK: - ThreadResourcesSheet

struct ThreadResourcesSheet: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord
    @Environment(\.dismiss) private var dismiss

    private var threadClaims: [Claim] {
        store.claims(for: thread.id)
    }

    private var evidenceEntries: [Entry] {
        store.topLevelEntries(for: thread.id).filter { $0.kind == .evidence }
    }

    private var sourceEntries: [Entry] {
        store.topLevelEntries(for: thread.id).filter { $0.kind == .source }
    }

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: TNSpacing.lg) {
                HStack(alignment: .top) {
                    VStack(alignment: .leading, spacing: TNSpacing.sm) {
                        Label("Thread Resources", systemImage: "books.vertical")
                            .font(.tnCaption.weight(.medium))
                            .foregroundStyle(.secondary)
                        Text(thread.title)
                            .font(.tnPageTitle)
                    }

                    Spacer()

                    if !store.lists.isEmpty {
                        Menu {
                            ForEach(store.lists.sorted { $0.updatedAt > $1.updatedAt }) { list in
                                Button {
                                    store.collectThreadResources(thread.id, into: list.id)
                                    store.selectList(list.id)
                                    store.closeThreadResources()
                                    dismiss()
                                } label: {
                                    Label(list.title, systemImage: listKindSymbol(for: list.kind))
                                }
                            }
                        } label: {
                            Label("Collect in List", systemImage: "plus.rectangle.on.folder")
                        }
                        .menuStyle(.borderlessButton)
                    }

                    Button("Done") {
                        store.closeThreadResources()
                        dismiss()
                    }
                    .buttonStyle(.bordered)
                }

                ResourceOverviewRow(items: resourceItems)

                if !threadClaims.isEmpty {
                    DocumentSection(title: "Claims", systemImage: "quote.bubble") {
                        ForEach(threadClaims) { claim in
                            VStack(alignment: .leading, spacing: TNSpacing.xs) {
                                Text(claim.statement)
                                    .font(.tnBody)
                                Text(claim.status.rawValue.capitalized)
                                    .font(.tnMicro)
                                    .foregroundStyle(.secondary)
                            }
                            .padding(.vertical, TNSpacing.xs)
                        }
                    }
                }

                if !evidenceEntries.isEmpty {
                    DocumentSection(title: "Evidence", systemImage: "checklist") {
                        ForEach(evidenceEntries) { entry in
                            ThreadResourceEntryRow(entry: entry)
                        }
                    }
                }

                if !sourceEntries.isEmpty {
                    DocumentSection(title: "Sources", systemImage: "bookmark") {
                        ForEach(sourceEntries) { entry in
                            ThreadResourceEntryRow(entry: entry)
                        }
                    }
                }
            }
            .padding(TNSpacing.lg)
        }
        .background(Color.tnBackground)
        .onDisappear {
            store.closeThreadResources()
        }
    }

    private var resourceItems: [ListResolvedItem] {
        let threadItem = ListResolvedItem(
            listItem: ListItem(
                id: thread.id, listID: thread.id, itemType: .thread, itemID: thread.id,
                addedAt: thread.updatedAt, note: nil, position: 0, isPinned: false
            ),
            thread: thread, entry: nil
        )
        let evidenceItems = evidenceEntries.map { entry in
            ListResolvedItem(
                listItem: ListItem(
                    id: entry.id, listID: thread.id, itemType: .entry, itemID: entry.id,
                    addedAt: entry.createdAt, note: nil, position: 0, isPinned: false
                ),
                thread: nil, entry: entry
            )
        }
        let sourceItems = sourceEntries.map { entry in
            ListResolvedItem(
                listItem: ListItem(
                    id: entry.id, listID: thread.id, itemType: .source, itemID: entry.id,
                    addedAt: entry.createdAt, note: nil, position: 0, isPinned: false
                ),
                thread: nil, entry: entry
            )
        }
        return [threadItem] + evidenceItems + sourceItems
    }
}

// MARK: - ThreadResourceEntryRow

struct ThreadResourceEntryRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            HStack {
                EntryKindBadge(kind: entry.kind)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }

            EntryBodyView(entry: entry)

            if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                Text(citation)
                    .font(.tnCaption)
                    .foregroundStyle(.secondary)
            }

            HStack(spacing: TNSpacing.sm) {
                if entry.isSourceResource {
                    Button("View Source") {
                        store.openSource(entry.id)
                    }
                    .buttonStyle(.bordered)
                    .controlSize(.mini)
                }

                Menu {
                    AddToListMenu(itemType: listItemType(for: entry), itemID: entry.id)
                } label: {
                    Label("Add to List", systemImage: "plus.rectangle.on.folder")
                        .font(.tnCaption)
                }
            }
        }
        .padding(.vertical, TNSpacing.sm)
    }
}
