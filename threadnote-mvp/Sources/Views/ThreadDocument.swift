import SwiftUI

struct ThreadDocument: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord
    let state: ThreadState?
    @State private var showingThreadMemory = false

    private var completionProvider: ThreadnoteCompletionProvider {
        ThreadnoteCompletionProvider(store: store)
    }

    var body: some View {
        @Bindable var store = store

        VStack(alignment: .leading, spacing: 0) {
            // Thread Title
            Text(thread.title)
                .font(.tnPageTitle)

            // Goal + Stage
            HStack(spacing: TNSpacing.sm) {
                if let subtitle = threadSecondarySummary(thread) {
                    Text(subtitle)
                        .font(.tnBody)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Text(thread.goalLayer.currentStage.title)
                    .font(.tnMicro.weight(.medium))
                    .padding(.horizontal, 8)
                    .padding(.vertical, 3)
                    .background(Color.accentColor.opacity(0.1), in: .capsule)
                    .foregroundStyle(Color.accentColor)
            }
            .padding(.bottom, TNSpacing.lg)

            if let state {
                threadContent(state)
            } else {
                Text("No thread state available yet.")
                    .foregroundStyle(.secondary)
                    .padding(.vertical, TNSpacing.xxl)
            }
        }
        .sheet(isPresented: $store.showingTimeline) {
            TimelineSheet(entries: store.visibleEntries(for: thread.id))
                .frame(minWidth: 520, minHeight: 520)
        }
        .popover(isPresented: $showingThreadMemory) {
            if let state {
                ThreadMemoryPopover(thread: thread, state: state)
                    .frame(minWidth: 400, minHeight: 300)
            }
        }
    }

    @ViewBuilder
    private func threadContent(_ state: ThreadState) -> some View {
        // Restart Note
        DocumentSection(title: "Restart Note", systemImage: "arrow.clockwise.circle") {
            restartNoteContent(state)
        }

        ThinDivider()

        // Continue (Inline Compose)
        DocumentSection(title: "Continue", systemImage: "text.cursor") {
            @Bindable var store = store
            CaptureEditorView(
                text: $store.inlineNoteDraft,
                helperText: "#role  @object  [[reference]]",
                submitLabel: "Add Note",
                minHeight: 100,
                submitAction: { store.appendInlineNote(to: thread.id) },
                completionProvider: completionProvider
            )
        }

        ThinDivider()

        // Working Stream
        DocumentSection(title: "Working Stream", systemImage: "note.text") {
            workingStreamContent(state)
        }

        ThinDivider()

        // Settled So Far (collapsed by default)
        InlineExpandable(title: "Settled So Far", count: state.resolvedSoFar.count) {
            settledContent(state)
        }

        ThinDivider()

        // Toolbar
        threadToolbar
    }

    // MARK: - Restart Note

    @ViewBuilder
    private func restartNoteContent(_ state: ThreadState) -> some View {
        let bulletItems: [String] = {
            if !state.recoveryLines.isEmpty {
                return state.recoveryLines.map { "\($0.title): \($0.body)" }
            }
            return state.restartNote
                .split(whereSeparator: \.isNewline)
                .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
                .filter { !$0.isEmpty }
        }()

        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            ForEach(Array(bulletItems.enumerated()), id: \.offset) { _, item in
                HStack(alignment: .top, spacing: TNSpacing.sm) {
                    Circle()
                        .fill(Color.accentColor.opacity(0.6))
                        .frame(width: 5, height: 5)
                        .padding(.top, 7)
                    Text(item)
                        .font(.tnBody)
                }
            }
            if let lastAnchorAt = state.lastAnchorAt {
                Text("Last saved \(lastAnchorAt.formatted(date: .abbreviated, time: .shortened))")
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }
        }
    }

    // MARK: - Working Stream

    @ViewBuilder
    private func workingStreamContent(_ state: ThreadState) -> some View {
        let noteItems = state.streamSections.flatMap(\.items)
        if noteItems.isEmpty {
            Text("Notes will appear here after the first capture.")
                .font(.tnCaption)
                .foregroundStyle(.tertiary)
        } else {
            ForEach(state.streamSections) { section in
                VStack(alignment: .leading, spacing: 0) {
                    Text(section.startedAt, format: .dateTime.month(.abbreviated).day())
                        .font(.tnMicro)
                        .foregroundStyle(.tertiary)
                        .padding(.bottom, TNSpacing.xs)

                    ForEach(section.items) { item in
                        EntryCard(item: item)
                            .padding(.vertical, TNSpacing.xs)
                    }
                }
            }
        }
    }

    // MARK: - Settled Content

    @ViewBuilder
    private func settledContent(_ state: ThreadState) -> some View {
        if state.resolvedSoFar.isEmpty {
            Text("No stable decisions saved yet.")
                .font(.tnCaption)
                .foregroundStyle(.tertiary)
        } else {
            ForEach(state.resolvedSoFar) { item in
                VStack(alignment: .leading, spacing: TNSpacing.xs) {
                    Text(item.text)
                        .font(.tnBody)
                    HStack(spacing: TNSpacing.sm) {
                        Text(ledgerLabel(for: item))
                            .font(.tnMicro.weight(.medium))
                            .foregroundStyle(.secondary)
                        Text(item.resolvedAt, format: .dateTime.month(.abbreviated).day())
                            .font(.tnMicro)
                            .foregroundStyle(.tertiary)
                    }
                }
                .padding(.vertical, TNSpacing.xs)
            }
        }
    }

    // MARK: - Toolbar

    private var threadToolbar: some View {
        HStack(spacing: TNSpacing.md) {
            Button("Resources") {
                store.openThreadResources(thread.id)
            }
            .buttonStyle(.bordered)
            .controlSize(.small)

            Button("Timeline") {
                store.showingTimeline = true
            }
            .buttonStyle(.bordered)
            .controlSize(.small)

            Button {
                showingThreadMemory.toggle()
            } label: {
                Label("Thread Memory", systemImage: "brain")
            }
            .buttonStyle(.bordered)
            .controlSize(.small)
            .keyboardShortcut("i", modifiers: .command)

            Spacer()

            ThreadToolsMenu(thread: thread)
        }
        .padding(.vertical, TNSpacing.md)
    }

    private func ledgerLabel(for item: ResolvedItem) -> String {
        switch item.statusLabel {
        case "Decided": "Decision Made"
        case "Confirmed": "Verified"
        case "Ruled out": "Dropped"
        default: item.statusLabel
        }
    }
}

// MARK: - ThreadMemoryPopover

struct ThreadMemoryPopover: View {
    let thread: ThreadRecord
    let state: ThreadState

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: TNSpacing.md) {
                Text("Thread Memory")
                    .font(.tnSectionTitle)

                if state.resolvedSoFar.isEmpty {
                    Text("No stable decisions saved yet.")
                        .font(.tnCaption)
                        .foregroundStyle(.tertiary)
                } else {
                    ForEach(state.resolvedSoFar) { item in
                        VStack(alignment: .leading, spacing: TNSpacing.xs) {
                            Text(item.text)
                                .font(.tnBody)
                            Text(item.resolvedAt, format: .dateTime.month(.abbreviated).day())
                                .font(.tnMicro)
                                .foregroundStyle(.tertiary)
                        }
                        ThinDivider()
                    }
                }
            }
            .padding(TNSpacing.md)
        }
    }
}

// MARK: - ThreadToolsMenu (moved from WorkbenchView)

struct ThreadToolsMenu: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    var body: some View {
        Menu {
            Button("Edit Goal", systemImage: "pencil") {
                store.beginGoalEditing(for: thread.id)
            }

            if thread.status == .active {
                Button("Archive Thread", systemImage: "archivebox") {
                    store.archiveThread(thread.id)
                }
            } else {
                Button("Restore Thread", systemImage: "arrow.uturn.backward") {
                    store.restoreThread(thread.id)
                }
            }

            Button("Thread Resources", systemImage: "books.vertical") {
                store.openThreadResources(thread.id)
            }

            Button("Full Timeline", systemImage: "clock") {
                store.showingTimeline = true
            }

            Menu("Prepare View") {
                ForEach(PreparedViewType.allCases) { type in
                    Button(type.title) {
                        store.prepareView(type: type, for: thread.id)
                    }
                }
            }

            if store.preparedView?.threadID == thread.id {
                Button("Exit Prepare", systemImage: "xmark.circle") {
                    store.preparedView = nil
                }
            }
        } label: {
            Image(systemName: "ellipsis.circle")
        }
        .menuStyle(.borderlessButton)
    }
}
