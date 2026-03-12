import SwiftUI

struct StreamDocument: View {
    @Environment(ThreadnoteStore.self) private var store
    @State private var draft = ""

    var body: some View {
        let provider = ThreadnoteCompletionProvider(store: store)

        VStack(alignment: .leading, spacing: 0) {
            Text("Stream")
                .font(.tnPageTitle)
                .padding(.bottom, TNSpacing.md)

            CaptureEditorView(
                text: $draft,
                helperText: "#role  @object  [[reference]]",
                submitLabel: "Send",
                minHeight: 120,
                submitAction: submitStreamCapture,
                completionProvider: provider
            )
            .padding(.bottom, TNSpacing.lg)

            LazyVStack(alignment: .leading, spacing: TNSpacing.sm + 2) {
                ForEach(store.streamEntries) { entry in
                    StreamEntryRow(entry: entry)
                }
            }
        }
    }

    private func submitStreamCapture() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        store.quickCaptureDraft = QuickCaptureDraft(text: text)
        store.submitCapture()
        draft = ""
    }
}

// MARK: - StreamEntryRow

struct StreamEntryRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry
    @State private var isHovered = false
    @State private var isEditing = false
    @State private var editText = ""

    private var isRouted: Bool { store.thread(for: entry) != nil }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            if isEditing {
                editingView
            } else {
                readingView
            }
        }
        .padding(.horizontal, TNSpacing.md + 4)
        .padding(.vertical, TNSpacing.md)
        .background(Color.tnSurface, in: .rect(cornerRadius: TNCorner.md))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.md)
                .stroke(isEditing ? Color.accentColor.opacity(0.3) : (isHovered ? Color.tnBorder : Color.tnBorderSubtle), lineWidth: 1)
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
        .contentShape(Rectangle())
        .onTapGesture(count: 2) { beginEditing() }
        .onTapGesture(count: 1) { /* single click: no-op */ }
        .onHover { hovering in isHovered = hovering }
        .contextMenu {
            Button("Edit") { beginEditing() }
            if let thread = store.thread(for: entry) {
                Button("Open Thread") { store.openThread(thread.id) }
            }
            if entry.isSourceResource {
                Button("View Source") { store.openSource(entry.id) }
            }
            Divider()
            AddToListMenu(itemType: listItemType(for: entry), itemID: entry.id)
        }
    }

    // MARK: - Reading view

    @ViewBuilder
    private var readingView: some View {
        // Tag pill + body
        HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
            Text("#\(entry.kind.rawValue)")
                .font(.tnCaption.weight(.medium))
                .foregroundStyle(entry.kind.kindColor)
                .padding(.horizontal, 7)
                .padding(.vertical, 2)
                .background(entry.kind.kindColor.opacity(0.1), in: .capsule)

            Text(entry.summaryText)
                .font(.tnBody)
        }

        // Footer: thread link + timestamp
        HStack(spacing: TNSpacing.sm) {
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

            Spacer()

            Text(entry.createdAt, style: .relative)
                .font(.tnMicro)
                .foregroundStyle(.quaternary)
        }

        // Unrouted: routing row
        if !isRouted {
            HStack(spacing: TNSpacing.sm) {
                let suggestions = store.suggestedThreads(for: entry, limit: 2)
                ForEach(suggestions) { suggestion in
                    Button {
                        store.resolveInboxEntry(entry, to: suggestion.thread.id)
                    } label: {
                        HStack(spacing: 4) {
                            Image(systemName: "sparkles")
                                .font(.system(size: 9))
                            Text(suggestion.thread.title)
                                .lineLimit(1)
                        }
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                        .padding(.horizontal, TNSpacing.sm)
                        .padding(.vertical, 5)
                        .background(Color.primary.opacity(0.04), in: .capsule)
                    }
                    .buttonStyle(.plain)
                }

                Spacer()

                if !store.homeThreads.isEmpty {
                    Menu {
                        ForEach(store.homeThreads) { thread in
                            Button(thread.title) {
                                store.resolveInboxEntry(entry, to: thread.id)
                            }
                        }
                    } label: {
                        Image(systemName: "arrow.turn.down.right")
                            .font(.tnCaption)
                            .foregroundStyle(.tertiary)
                            .frame(width: 28, height: 28)
                            .contentShape(Rectangle())
                    }
                    .menuStyle(.borderlessButton)
                    .frame(maxWidth: 28)
                }

                Button {
                    store.createThreadFromEntry(entry.id)
                } label: {
                    Image(systemName: "plus")
                        .font(.tnCaption)
                        .foregroundStyle(.tertiary)
                        .frame(width: 28, height: 28)
                        .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
            }
        }
    }

    // MARK: - Editing view

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

    // MARK: - Actions

    private func beginEditing() {
        editText = entry.summaryText
        isEditing = true
    }

    private func commitEdit() {
        store.updateEntryText(entry.id, newText: editText)
        isEditing = false
    }
}
