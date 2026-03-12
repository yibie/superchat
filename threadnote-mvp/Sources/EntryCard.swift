import SwiftUI

// MARK: - EntryCard

struct EntryCard: View {
    @Environment(ThreadnoteStore.self) private var store
    let item: EntryStreamItem

    private var referenceItems: [ReferenceDisplayItem] {
        let noteReferences = store.referencedEntries(for: item.entry).map {
            ReferenceDisplayItem(id: "note-\($0.id.uuidString)", label: $0.summaryText, systemImage: "note.text", tint: .blue)
        }
        let threadReferences = store.referencedThreads(for: item.entry).map {
            ReferenceDisplayItem(id: "thread-\($0.id.uuidString)", label: $0.title, systemImage: "rectangle.stack", tint: .purple)
        }
        let unresolvedReferences = store.unresolvedReferences(for: item.entry).map {
            ReferenceDisplayItem(id: "unresolved-\($0.id.uuidString)", label: $0.label, systemImage: "questionmark.square.dashed", tint: .secondary)
        }
        return noteReferences + threadReferences + unresolvedReferences
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.xs) {
            // Tag pill + body text
            HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
                Text("#\(item.entry.kind.rawValue)")
                    .font(.tnCaption.weight(.medium))
                    .foregroundStyle(item.entry.kind.kindColor)
                    .padding(.horizontal, 7)
                    .padding(.vertical, 2)
                    .background(item.entry.kind.kindColor.opacity(0.1), in: .capsule)

                Text(item.entry.summaryText)
                    .font(.tnBody)
            }

            // Citation if present
            if let citation = item.entry.sourceMetadata?.citation {
                Text(citation)
                    .font(.tnCaption)
                    .foregroundStyle(.tertiary)
            }

            // Metadata line: relation + references + timestamp + actions
            HStack(spacing: TNSpacing.sm) {
                if let relation = item.primaryRelation {
                    RelationBadge(kind: relation.kind)
                }

                if !referenceItems.isEmpty {
                    ForEach(referenceItems) { ref in
                        Text("[\(ref.label)]")
                            .font(.tnMicro)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                }

                Spacer()

                Text(item.entry.createdAt, style: .relative)
                    .font(.tnMicro)
                    .foregroundStyle(.quaternary)

                if item.entry.isSourceResource {
                    Button {
                        store.openSource(item.entry.id)
                    } label: {
                        Image(systemName: "bookmark")
                            .font(.system(size: 9))
                    }
                    .buttonStyle(.plain)
                    .foregroundStyle(.tertiary)
                }

                Button {
                    store.toggleReplies(for: item.entry.id)
                } label: {
                    HStack(spacing: 2) {
                        Image(systemName: "arrowshape.turn.up.left")
                            .font(.system(size: 9))
                        if !item.replies.isEmpty {
                            Text("\(item.replies.count)")
                                .font(.tnMicro)
                        }
                    }
                }
                .buttonStyle(.plain)
                .foregroundStyle(.tertiary)
            }

            if store.expandedReplyEntryIDs.contains(item.entry.id) {
                replySection
            }
        }
        .padding(.vertical, TNSpacing.xs)
        .contextMenu {
            AddToListMenu(itemType: listItemType(for: item.entry), itemID: item.entry.id)
        }
    }

    @ViewBuilder
    private var replySection: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            if !item.replies.isEmpty {
                ForEach(item.replies) { reply in
                    ReplyRow(entry: reply)
                }
            }

            TextEditor(text: Binding(
                get: { store.replyDrafts[item.entry.id, default: ""] },
                set: { store.replyDrafts[item.entry.id] = $0 }
            ))
            .font(.tnBody)
            .scrollContentBackground(.hidden)
            .frame(minHeight: 56, maxHeight: 100)
            .padding(TNSpacing.sm)
            .background(Color.tnSurface.opacity(0.5), in: .rect(cornerRadius: TNCorner.sm))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.sm)
                    .stroke(Color.tnBorder, lineWidth: 1)
            )

            HStack {
                Spacer()
                Button("Send Reply") {
                    store.appendReply(to: item.entry.id)
                }
                .buttonStyle(.borderedProminent)
                .controlSize(.small)
                .disabled(store.replyDrafts[item.entry.id, default: ""].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .padding(.leading, TNSpacing.md)
    }
}

// MARK: - ReferenceDisplayItem

struct ReferenceDisplayItem: Identifiable {
    let id: String
    let label: String
    let systemImage: String
    let tint: Color
}

// MARK: - ReferenceChip

struct ReferenceChip: View {
    let reference: ReferenceDisplayItem

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: reference.systemImage)
            Text(reference.label)
                .lineLimit(1)
        }
        .font(.tnMicro)
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .background(reference.tint.opacity(0.08), in: .capsule)
    }
}

// MARK: - ReplyRow

struct ReplyRow: View {
    let entry: Entry

    var body: some View {
        HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
            Text("#\(entry.kind.rawValue)")
                .font(.tnMicro.weight(.medium))
                .foregroundStyle(entry.kind.kindColor)
                .padding(.horizontal, 5)
                .padding(.vertical, 1)
                .background(entry.kind.kindColor.opacity(0.1), in: .capsule)

            Text(entry.summaryText)
                .font(.tnCaption)

            Spacer()

            Text(entry.createdAt, style: .relative)
                .font(.tnMicro)
                .foregroundStyle(.quaternary)
        }
        .padding(.vertical, TNSpacing.xs)
    }
}
