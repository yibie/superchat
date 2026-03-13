import SwiftUI

// MARK: - NoteCard (standard card — used in non-timeline contexts)

struct NoteCard: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry
    var isTimelineMode: Bool = false
    @State private var isHovered = false
    @State private var isEditing = false
    @State private var editText = ""

    private var isRouted: Bool { store.thread(for: entry) != nil }

    private var relation: DiscourseRelation? {
        primaryRelation(for: entry.id, relations: store.discourseRelations)
    }

    private var replies: [Entry] {
        store.replies(for: entry.id)
    }

    private var referenceItems: [ReferenceDisplayItem] {
        let noteRefs = store.referencedEntries(for: entry).map {
            ReferenceDisplayItem(id: "note-\($0.id.uuidString)", label: $0.summaryText, systemImage: "note.text", tint: .blue)
        }
        let threadRefs = store.referencedThreads(for: entry).map {
            ReferenceDisplayItem(id: "thread-\($0.id.uuidString)", label: $0.title, systemImage: "rectangle.stack", tint: .purple)
        }
        let unresolvedRefs = store.unresolvedReferences(for: entry).map {
            ReferenceDisplayItem(id: "unresolved-\($0.id.uuidString)", label: $0.label, systemImage: "questionmark.square.dashed", tint: .secondary)
        }
        return noteRefs + threadRefs + unresolvedRefs
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            if isEditing {
                editingView
            } else {
                readingView
            }
        }
        .padding(.horizontal, TNSpacing.md + 6)
        .padding(.vertical, TNSpacing.md + 6)
        .frame(maxWidth: .infinity, minHeight: 80, alignment: .leading)
        .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.md))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.md)
                .stroke(isEditing ? Color.accentColor.opacity(0.3) : Color.tnBorderSubtle, lineWidth: 1)
        )
        .overlay(alignment: .leading) {
            if !isRouted && !isEditing {
                UnevenRoundedRectangle(
                    topLeadingRadius: TNCorner.md,
                    bottomLeadingRadius: TNCorner.md,
                    bottomTrailingRadius: 0,
                    topTrailingRadius: 0
                )
                .fill(Color.orange.opacity(0.35))
                .frame(width: 3)
            }
        }
        .overlay(alignment: .topTrailing) {
            if isHovered && !isEditing {
                Menu {
                    Button("Edit") { beginEditing() }
                    if entry.isSourceResource {
                        Button("View Source") { store.openSource(entry.id) }
                    }
                    if let thread = store.thread(for: entry) {
                        Button("Open Thread") { store.openThread(thread.id) }
                    }
                    if !isRouted {
                        Divider()
                        let suggestions = store.suggestedThreads(for: entry, limit: 3)
                        if !suggestions.isEmpty {
                            ForEach(suggestions) { suggestion in
                                Button {
                                    store.resolveInboxEntry(entry, to: suggestion.thread.id)
                                } label: {
                                    Label(suggestion.thread.title, systemImage: "sparkles")
                                }
                            }
                            Divider()
                        }
                        if !store.homeThreads.isEmpty {
                            Menu("Route to Thread") {
                                ForEach(store.homeThreads) { thread in
                                    Button(thread.title) {
                                        store.resolveInboxEntry(entry, to: thread.id)
                                    }
                                }
                            }
                        }
                        Button {
                            store.createThreadFromEntry(entry.id)
                        } label: {
                            Label("New Thread", systemImage: "plus")
                        }
                    }
                } label: {
                    Image(systemName: "ellipsis")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                        .frame(width: 24, height: 24)
                        .contentShape(Rectangle())
                }
                .menuStyle(.borderlessButton)
                .menuIndicator(.hidden)
                .frame(maxWidth: 24)
                .padding(TNSpacing.xs)
                .transition(.opacity)
            }
        }
        .animation(.spring(response: 0.25, dampingFraction: 0.8), value: isHovered)
        .contentShape(Rectangle())
        .onTapGesture(count: 2) { beginEditing() }
        .onHover { hovering in isHovered = hovering }
        .contextMenu {
            Button("Edit") { beginEditing() }
            if let thread = store.thread(for: entry) {
                Button("Open Thread") { store.openThread(thread.id) }
            }
            if entry.isSourceResource {
                Button("View Source") { store.openSource(entry.id) }
            }
        }
    }

    @ViewBuilder
    private var readingView: some View {
        Text(entryAttributedText(kind: entry.kind, text: entry.summaryText, font: .tnBody))
            .font(.tnBody)
            .lineSpacing(3)
            .fixedSize(horizontal: false, vertical: true)

        if entry.body.kind != .text {
            RichBodyView(entry: entry)
                .onAppear { store.enrichEntryMetadata(entry) }
        }

        if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
            Text(citation)
                .font(.tnCaption)
                .foregroundStyle(.tertiary)
        }

        if relation != nil || !referenceItems.isEmpty {
            HStack(spacing: TNSpacing.sm) {
                if let relation {
                    RelationBadge(kind: relation.kind)
                }
                ForEach(referenceItems) { ref in
                    Text("[\(ref.label)]")
                        .font(.tnMicro)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                }
            }
        }

        if let thread = store.thread(for: entry) {
            Button {
                store.openThread(thread.id)
            } label: {
                HStack(spacing: 3) {
                    Image(systemName: "arrow.turn.down.right")
                        .font(.system(size: 9))
                    Text(thread.title)
                        .lineLimit(1)
                }
                .font(.tnMicro)
                .foregroundStyle(.secondary)
            }
            .buttonStyle(.plain)
        }

        if store.expandedReplyEntryIDs.contains(entry.id) {
            replySection
        }
    }

    @ViewBuilder
    private var editingView: some View {
        TextEditor(text: $editText)
            .font(.tnBody)
            .scrollContentBackground(.hidden)
            .frame(minHeight: 60, maxHeight: 200)

        HStack {
            Text("Esc to cancel, ⌘↩ to save")
                .font(.tnMicro)
                .foregroundStyle(.tertiary)
            Spacer()
            Button("Cancel") { isEditing = false }
                .buttonStyle(.plain)
                .font(.tnCaption)
                .foregroundStyle(.secondary)
            Button("Save") { commitEdit() }
                .buttonStyle(.borderedProminent)
                .controlSize(.small)
                .keyboardShortcut(.return, modifiers: .command)
        }
    }

    @ViewBuilder
    private var replySection: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            let replies = store.replies(for: entry.id)
            if !replies.isEmpty {
                ForEach(replies) { reply in
                    ReplyRow(entry: reply)
                }
            }

            TextEditor(text: Binding(
                get: { store.replyDrafts[entry.id, default: ""] },
                set: { store.replyDrafts[entry.id] = $0 }
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
                    store.appendReply(to: entry.id)
                }
                .buttonStyle(.borderedProminent)
                .controlSize(.small)
                .disabled(store.replyDrafts[entry.id, default: ""].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .padding(.leading, TNSpacing.md)
    }

    private func beginEditing() {
        editText = entry.summaryText
        isEditing = true
    }

    private func commitEdit() {
        store.updateEntryText(entry.id, newText: editText)
        isEditing = false
    }
}

// MARK: - TimelineEntryRow
//
// Renders one entry + its replies as a visual group.
// Distill-style: colored dot for routed, hollow gray for unrouted.
// Vertical line connects the root entry to all replies in the group.
// Action buttons (reply + route) always visible below content.

struct TimelineEntryRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry
    let threadColor: Color?   // nil = use entry.kind.kindColor; non-nil = thread identity color
    let isLast: Bool

    private static let gutterWidth: CGFloat = 10
    private static let gutterSpacing: CGFloat = 10

    private var isRouted: Bool { store.thread(for: entry) != nil }

    private var dotColor: Color {
        if let c = threadColor { return c }
        return isRouted ? entry.kind.kindColor : Color.primary.opacity(0.25)
    }

    private var replies: [Entry] { store.replies(for: entry.id) }

    @ViewBuilder
    private var dotView: some View {
        ZStack {
            if store.routingEntryIDs.contains(entry.id) {
                ProgressView()
                    .controlSize(.mini)
            } else if isRouted {
                Circle().fill(dotColor)
            } else {
                Circle().stroke(Color.primary.opacity(0.3), lineWidth: 1.5)
                Circle().fill(Color.primary.opacity(0.06))
            }
        }
        .frame(width: Self.gutterWidth, height: Self.gutterWidth)
        .animation(.easeInOut(duration: 0.3), value: store.routingEntryIDs.contains(entry.id))
    }

    var body: some View {
        let gw = Self.gutterWidth
        let gs = Self.gutterSpacing
        let hasReplies = !replies.isEmpty
        let isExpanded = store.expandedReplyEntryIDs.contains(entry.id)

        VStack(alignment: .leading, spacing: 0) {
            // Root entry — dot | content
            HStack(alignment: .top, spacing: gs) {
                dotView.padding(.top, 5).frame(width: gw)
                TimelineRowContent(entry: entry, threadColor: threadColor)
            }

            // Replies
            if hasReplies && isExpanded {
                ForEach(replies) { reply in
                    HStack(alignment: .top, spacing: gs) {
                        dotView.padding(.top, 5).frame(width: gw)
                        TimelineRowContent(entry: reply, threadColor: threadColor)
                    }
                }
            }

            // Reply draft box
            if isExpanded {
                HStack(alignment: .top, spacing: gs) {
                    Spacer().frame(width: gw)
                    replyComposer
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        // Vertical connector line — only when replies expanded
        .background(alignment: .topLeading) {
            if hasReplies && isExpanded {
                Rectangle()
                    .fill(dotColor.opacity(0.35))
                    .frame(width: 1)
                    .frame(maxHeight: .infinity)
                    .padding(.leading, gw / 2)
                    .padding(.top, 5 + gw + 2)
            }
        }
        .padding(.vertical, TNSpacing.sm)
    }

    @State private var replyEditorHeight: CGFloat = 48

    @ViewBuilder
    private var replyComposer: some View {
        VStack(alignment: .leading, spacing: TNSpacing.xs) {
            RichCaptureEditor(
                text: Binding(
                    get: { store.replyDrafts[entry.id, default: ""] },
                    set: { store.replyDrafts[entry.id] = $0 }
                ),
                placeholder: "Reply…",
                minHeight: 48,
                onSubmit: { store.appendReply(to: entry.id) },
                onHeightChange: { h in replyEditorHeight = min(h, 200) }
            )
            .frame(height: replyEditorHeight)
            .background(Color.tnSurface.opacity(0.6), in: .rect(cornerRadius: TNCorner.sm))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.sm)
                    .stroke(Color.tnBorder, lineWidth: 1)
            )

            HStack {
                Text("⌘↩ to send").font(.tnMicro).foregroundStyle(.tertiary)
                Spacer()
                Button("Send") { store.appendReply(to: entry.id) }
                    .buttonStyle(.borderedProminent)
                    .controlSize(.small)
                    .disabled(store.replyDrafts[entry.id, default: ""].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .padding(.bottom, TNSpacing.xs)
    }
}

// MARK: - TimelineRowContent (inline, no card wrapper)

private struct TimelineRowContent: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry
    let threadColor: Color?

    @State private var isHovered = false
    @State private var isEditing = false
    @State private var editText = ""

    private var isRouted: Bool { store.thread(for: entry) != nil }
    private var isRich: Bool { entry.body.kind == .url || entry.body.kind == .image }
    private var showActions: Bool { isHovered || store.expandedReplyEntryIDs.contains(entry.id) }

    private var relation: DiscourseRelation? {
        primaryRelation(for: entry.id, relations: store.discourseRelations)
    }
    private var referenceItems: [ReferenceDisplayItem] {
        let noteRefs = store.referencedEntries(for: entry).map {
            ReferenceDisplayItem(id: "note-\($0.id.uuidString)", label: $0.summaryText, systemImage: "note.text", tint: .blue)
        }
        let threadRefs = store.referencedThreads(for: entry).map {
            ReferenceDisplayItem(id: "thread-\($0.id.uuidString)", label: $0.title, systemImage: "rectangle.stack", tint: .purple)
        }
        let unresolvedRefs = store.unresolvedReferences(for: entry).map {
            ReferenceDisplayItem(id: "unresolved-\($0.id.uuidString)", label: $0.label, systemImage: "questionmark.square.dashed", tint: .secondary)
        }
        return noteRefs + threadRefs + unresolvedRefs
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.xs) {
            if isEditing {
                editingView
            } else {
                readingView
            }
        }
        .padding(.bottom, TNSpacing.xs)
        .overlay(alignment: .leading) {
            if !isRouted && !store.routingEntryIDs.contains(entry.id) {
                UnevenRoundedRectangle(
                    topLeadingRadius: TNCorner.md,
                    bottomLeadingRadius: TNCorner.md,
                    bottomTrailingRadius: 0,
                    topTrailingRadius: 0
                )
                .fill(Color.orange.opacity(0.35))
                .frame(width: 2)
            }
        }
        .contentShape(Rectangle())
        .onTapGesture(count: 2) {
            editText = entry.summaryText
            isEditing = true
        }
        .onHover { isHovered = $0 }
    }

    @ViewBuilder
    private var readingView: some View {
        HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
            Text(entryAttributedText(kind: entry.kind, text: entry.summaryText, font: .tnBody))
                .font(.tnBody)
                .lineSpacing(2)
                .fixedSize(horizontal: false, vertical: true)
                .textSelection(.enabled)
                .frame(maxWidth: .infinity, alignment: .leading)
            Text(entry.createdAt.formatted(date: .omitted, time: .shortened))
                .font(.tnMicro)
                .foregroundStyle(.tertiary)
                .fixedSize()
        }

        // Rich body (URL/image)
        if isRich {
            RichBodyView(entry: entry)
                .onAppear { store.enrichEntryMetadata(entry) }
                .padding(TNSpacing.sm)
                .background(Color.tnSurface.opacity(0.7), in: .rect(cornerRadius: TNCorner.sm))
                .overlay(
                    RoundedRectangle(cornerRadius: TNCorner.sm)
                        .stroke(Color.tnBorderSubtle, lineWidth: 1)
                )
        }

        // Citation
        if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
            Text(citation)
                .font(.tnCaption)
                .foregroundStyle(.tertiary)
        }

        // Relation badge + reference pills
        if relation != nil || !referenceItems.isEmpty {
            HStack(spacing: TNSpacing.sm) {
                if let relation {
                    RelationBadge(kind: relation.kind)
                }
                ForEach(referenceItems) { ref in
                    Text("[\(ref.label)]")
                        .font(.tnMicro)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                }
            }
        }

        // Thread tag
        if isRouted, let thread = store.thread(for: entry) {
            Button { store.openThread(thread.id) } label: {
                HStack(spacing: 4) {
                    Circle().fill(thread.color.color).frame(width: 5, height: 5)
                    Text(thread.title).lineLimit(1)
                }
                .font(.tnCaption)
                .foregroundStyle(.secondary)
            }
            .buttonStyle(.plain)
        }

        // Action row
        HStack(spacing: TNSpacing.xs) {
            Button {
                store.toggleReplies(for: entry.id)
            } label: {
                let replies = store.replies(for: entry.id)
                Text(replies.isEmpty ? "Reply" : "Reply · \(replies.count)")
                    .foregroundStyle(store.expandedReplyEntryIDs.contains(entry.id) ? Color.accentColor : Color.secondary)
                    .actionPill()
            }
            .buttonStyle(.plain)

            if !isRouted {
                if store.routingFailedEntryIDs.contains(entry.id) {
                    Text("AI 无法归类")
                        .font(.tnCaption)
                        .foregroundStyle(.orange)
                }
                Menu {
                    ForEach(store.homeThreads) { thread in
                        Button(thread.title) {
                            store.resolveInboxEntry(entry, to: thread.id)
                        }
                    }
                    Divider()
                    Button { store.createThreadFromEntry(entry.id) } label: {
                        Label("New Thread", systemImage: "plus")
                    }
                } label: {
                    Text("Add to thread")
                        .foregroundStyle(.secondary)
                        .actionPill()
                }
                .menuStyle(.borderlessButton)
                .menuIndicator(.hidden)
                .fixedSize()
            }

            Spacer()

            Menu {
                Button("Edit") {
                    editText = entry.summaryText
                    isEditing = true
                }
                if entry.isSourceResource {
                    Button("View Source") { store.openSource(entry.id) }
                }
            } label: {
                Text("···")
                    .foregroundStyle(.tertiary)
                    .actionPill()
            }
            .menuStyle(.borderlessButton)
            .menuIndicator(.hidden)
            .fixedSize()
        }
        .font(.tnCaption)
        .padding(.top, TNSpacing.xs)
        .opacity(showActions ? 1 : 0)
    }

    @ViewBuilder
    private var editingView: some View {
        TextEditor(text: $editText)
            .font(.tnBody)
            .scrollContentBackground(.hidden)
            .frame(minHeight: 60, maxHeight: 200)

        HStack {
            Text("Esc to cancel · ⌘↩ to save")
                .font(.tnMicro)
                .foregroundStyle(.tertiary)
            Spacer()
            Button("Cancel") { isEditing = false }
                .buttonStyle(.plain)
                .font(.tnCaption)
                .foregroundStyle(.secondary)
            Button("Save") { commitEdit() }
                .buttonStyle(.borderedProminent)
                .controlSize(.small)
                .keyboardShortcut(.return, modifiers: .command)
        }
    }

    private func commitEdit() {
        store.updateEntryText(entry.id, newText: editText)
        isEditing = false
    }
}

// MARK: - ReferenceDisplayItem

struct ReferenceDisplayItem: Identifiable {
    let id: String
    let label: String
    let systemImage: String
    let tint: Color
}

// MARK: - Action pill style

private extension View {
    func actionPill(tint: Color = Color.primary.opacity(0.06)) -> some View {
        self
            .padding(.horizontal, TNSpacing.sm)
            .padding(.vertical, 3)
            .background(tint, in: Capsule())
            .overlay(Capsule().stroke(Color.tnBorderSubtle, lineWidth: 1))
    }
}

// MARK: - ReplyRow (used in NoteCard non-timeline mode)

struct ReplyRow: View {
    let entry: Entry

    var body: some View {
        HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
            Text(entryAttributedText(kind: entry.kind, text: entry.summaryText, font: .tnCaption))
                .font(.tnCaption)
            Spacer()
        }
        .padding(.vertical, TNSpacing.xs)
    }
}
