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
        VStack(alignment: .leading, spacing: 0) {
            threadToolbar
                .padding(.bottom, TNSpacing.lg)

            HStack(alignment: .firstTextBaseline, spacing: TNSpacing.sm) {
                Circle()
                    .fill(thread.color.color)
                    .frame(width: 10, height: 10)
                    .offset(y: -2)
                Text(thread.title)
                    .font(.tnPageTitle)
            }

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
        .popover(isPresented: $showingThreadMemory) {
            ThreadMemoryPopover(thread: thread)
        }
    }

    @ViewBuilder
    private func threadContent(_ state: ThreadState) -> some View {
        DocumentSection(title: "Restart Note", systemImage: "arrow.clockwise.circle") {
            restartNoteContent(state)
        }

        ThinDivider()

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

        DocumentSection(title: "Working Stream", systemImage: "note.text") {
            workingStreamContent(state)
        }
    }

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

                    let sectionItems = section.items.filter { $0.entry.parentEntryID == nil }
                    let lastItem = sectionItems.last
                    ForEach(sectionItems) { item in
                        TimelineEntryRow(
                            entry: item.entry,
                            threadColor: thread.color.color,
                            isLast: item.id == lastItem?.id
                        )
                    }
                }
            }
        }
    }

    private var threadToolbar: some View {
        HStack(spacing: TNSpacing.sm) {
            ThreadChromeButton(
                title: "Thread Memory",
                systemImage: "brain",
                isActive: showingThreadMemory
            ) {
                showingThreadMemory.toggle()
            }
            .keyboardShortcut("i", modifiers: .command)

            Spacer(minLength: TNSpacing.md)

            ThreadToolsMenu(thread: thread)
        }
        .padding(.vertical, TNSpacing.xs)
    }
}

struct ThreadMemoryPopover: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: TNSpacing.lg) {
                Text("Thread Memory")
                    .font(.tnSectionTitle)
                    .padding(.bottom, TNSpacing.xs)

                MemorySectionView(
                    title: "Session",
                    emptyText: "No working notes yet.",
                    records: store.memoryRecords(for: thread.id, scope: .working)
                )
                MemorySectionView(
                    title: "Stable",
                    emptyText: "No settled decisions yet.",
                    records: store.memoryRecords(for: thread.id, scope: .semantic)
                        + store.memoryRecords(for: thread.id, scope: .episodic)
                )
                MemorySectionView(
                    title: "Sources",
                    emptyText: "No sources logged yet.",
                    records: store.memoryRecords(for: thread.id, scope: .source)
                )
            }
            .padding(TNSpacing.md)
        }
        .frame(width: 280)
    }
}

private struct MemorySectionView: View {
    let title: String
    let emptyText: String
    let records: [MemoryRecord]

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.xs) {
            Text(title)
                .font(.tnCaption)
                .foregroundStyle(.secondary)
                .textCase(.uppercase)

            if records.isEmpty {
                Text(emptyText)
                    .font(.tnCaption)
                    .foregroundStyle(.tertiary)
                    .padding(.top, 2)
            } else {
                ForEach(records.prefix(8)) { record in
                    VStack(alignment: .leading, spacing: 2) {
                        Text(record.text)
                            .font(.tnBody)
                            .lineLimit(2)
                        HStack(spacing: 4) {
                            Text(record.scope.label)
                                .font(.tnMicro)
                                .foregroundStyle(.tertiary)
                            Text("·")
                                .font(.tnMicro)
                                .foregroundStyle(.tertiary)
                            Text(record.createdAt, format: .dateTime.month(.abbreviated).day().hour().minute())
                                .font(.tnMicro)
                                .foregroundStyle(.tertiary)
                        }
                    }
                    .padding(.vertical, 2)
                    ThinDivider()
                }
            }
        }
    }
}

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
                store.openThreadSidebar(.resources, for: thread.id)
            }

            Button("Full Timeline", systemImage: "clock") {
                store.openThreadSidebar(.timeline, for: thread.id)
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
            Label("Thread Tools", systemImage: "ellipsis.circle")
                .font(.tnCaption.weight(.medium))
                .foregroundStyle(.primary)
                .padding(.horizontal, 12)
                .padding(.vertical, 8)
                .background(Color.tnBackground, in: Capsule())
                .overlay(
                    Capsule()
                        .stroke(Color.tnBorder, lineWidth: 1)
                )
        }
        .menuStyle(.borderlessButton)
    }
}

private struct ThreadChromeButton: View {
    let title: String
    let systemImage: String
    let isActive: Bool
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            HStack(spacing: TNSpacing.xs) {
                Image(systemName: systemImage)
                Text(title)
            }
            .font(.tnCaption.weight(.medium))
            .foregroundStyle(isActive ? Color.accentColor : .primary)
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(backgroundStyle, in: Capsule())
            .overlay(
                Capsule()
                    .stroke(isActive ? Color.accentColor.opacity(0.2) : Color.tnBorder, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }

    private var backgroundStyle: Color {
        isActive ? Color.accentColor.opacity(0.12) : Color.tnBackground
    }
}
