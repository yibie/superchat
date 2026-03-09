import AppKit
import SwiftUI

private enum HomeSection: String, CaseIterable, Identifiable {
    case capture
    case resume
    case lists
    case sorting
    case recent

    var id: String { rawValue }

    var title: String {
        switch self {
        case .capture:
            "Quick Capture"
        case .resume:
            "Return to a Thread"
        case .lists:
            "Lists"
        case .sorting:
            "Needs Sorting"
        case .recent:
            "Recent Notes"
        }
    }

    var systemImage: String {
        switch self {
        case .capture:
            "square.and.pencil"
        case .resume:
            "arrow.clockwise.circle"
        case .lists:
            "rectangle.stack"
        case .sorting:
            "tray"
        case .recent:
            "note.text"
        }
    }
}

struct ContentView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.openWindow) private var openWindow
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.colorScheme) private var colorScheme

    var body: some View {
        VStack(spacing: 0) {
            header
            Divider()

            Group {
                if store.route == .thread, let thread = store.selectedThread {
                    ThreadPageView(store: store, thread: thread)
                } else {
                    HomeView(store: store, openQuickCapture: openQuickCapture)
                }
            }
            .padding(14)
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
        .background(
            LinearGradient(
                colors: [
                    Color(nsColor: .windowBackgroundColor),
                    Color(nsColor: colorScheme == .dark ? .underPageBackgroundColor : .controlBackgroundColor)
                ],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            )
        )
        .toolbar {
            ToolbarItem {
                Button("Capture", systemImage: "plus.bubble") {
                    openQuickCapture()
                }
                .keyboardShortcut("n", modifiers: [.command, .shift])
            }
        }
        .sheet(item: $store.preparedView) { _ in
            DraftSheetView(store: store)
        }
        .sheet(item: presentedListBinding) { _ in
            ListSheetView(store: store)
        }
        .onChange(of: scenePhase) { _, newPhase in
            if newPhase != .active {
                store.maybeWriteAnchorIfNeeded(for: store.selectedThreadID)
            }
        }
    }

    private var header: some View {
        HStack(spacing: 16) {
            if store.route == .thread {
                Button("Home", systemImage: "chevron.left") {
                    store.goHome()
                }
                .buttonStyle(.borderless)
            }

            VStack(alignment: .leading, spacing: 2) {
                Text(store.route == .thread ? (store.selectedThread?.title ?? "Thread") : "Threadnote")
                    .font(.title2.bold())
                Text(headerSubtitle)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            Spacer()

            if let thread = store.selectedThread, store.route == .thread {
                Text(thread.lastActiveAt, style: .relative)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            Button("Quick Capture", systemImage: "bolt.circle") {
                openQuickCapture()
            }
            .buttonStyle(.borderedProminent)
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 14)
        .background(.bar)
    }

    private var headerSubtitle: String {
        switch store.route {
        case .home:
            return "Capture first. Return to a thread only when you want to continue."
        case .thread:
            return store.selectedThread?.prompt ?? "Resume one problem at a time."
        }
    }

    private func openQuickCapture() {
        store.beginCapture()
        openWindow(id: "capture")
    }

    private var presentedListBinding: Binding<ListRecord?> {
        Binding(
            get: { store.presentedList },
            set: { store.presentedListID = $0?.id }
        )
    }
}

private struct HomeView: View {
    @Bindable var store: ThreadnoteStore
    let openQuickCapture: () -> Void

    var body: some View {
        ScrollViewReader { proxy in
            HSplitView {
                HomeSidebarView(store: store) { section in
                    withAnimation(.easeInOut(duration: 0.2)) {
                        proxy.scrollTo(section.id, anchor: .top)
                    }
                }
                .frame(minWidth: 220, idealWidth: 240, maxWidth: 260)

                HomeContentView(store: store, openQuickCapture: openQuickCapture)
            }
        }
    }
}

private struct HomeSidebarView: View {
    @Bindable var store: ThreadnoteStore
    let onSelectSection: (HomeSection) -> Void

    var body: some View {
        SidebarSurface {
            VStack(alignment: .leading, spacing: 18) {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Home")
                        .font(.headline)
                    Text("Navigate the current workspace without losing the recent note stream.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }

                VStack(alignment: .leading, spacing: 8) {
                    ForEach(HomeSection.allCases) { section in
                        Button {
                            onSelectSection(section)
                        } label: {
                            HStack(spacing: 10) {
                                Image(systemName: section.systemImage)
                                Text(section.title)
                                Spacer()
                                Text(badgeValue(for: section))
                                    .font(.caption2)
                                    .foregroundStyle(.secondary)
                            }
                            .padding(.horizontal, 12)
                            .padding(.vertical, 10)
                            .background(.thinMaterial, in: .rect(cornerRadius: 14))
                        }
                        .buttonStyle(.plain)
                    }
                }

                Spacer()

                if let thread = store.homeThreads.first {
                    VStack(alignment: .leading, spacing: 6) {
                        Text("Resume First")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                        Button {
                            store.openThread(thread.id)
                        } label: {
                            VStack(alignment: .leading, spacing: 4) {
                                Text(thread.title)
                                    .font(.subheadline.weight(.semibold))
                                Text(thread.nextStep)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                            }
                            .frame(maxWidth: .infinity, alignment: .leading)
                            .padding(12)
                            .background(.thinMaterial, in: .rect(cornerRadius: 14))
                        }
                        .buttonStyle(.plain)
                    }
                }
            }
        }
    }

    private func badgeValue(for section: HomeSection) -> String {
        switch section {
        case .capture:
            return "Now"
        case .resume:
            return "\(store.homeThreads.count)"
        case .lists:
            return "\(store.homeLists.count)"
        case .sorting:
            return "\(store.needsSortingEntries.count)"
        case .recent:
            return "\(store.homeRecentEntries.count)"
        }
    }
}

private struct HomeContentView: View {
    @Bindable var store: ThreadnoteStore
    let openQuickCapture: () -> Void

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 14) {
                SectionCard(title: "Quick Capture", systemImage: "square.and.pencil") {
                    TextEditor(text: $store.noteStreamDraft)
                        .font(.body)
                        .scrollContentBackground(.hidden)
                        .frame(minHeight: 96)
                        .padding(10)
                        .background(Color(nsColor: .textBackgroundColor), in: .rect(cornerRadius: 14))

                    HStack {
                        Text("Write first. The system can route it afterward.")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                        Spacer()
                        Button("Capture") {
                            store.submitNoteStreamCapture()
                        }
                        .buttonStyle(.borderedProminent)

                        Button("Popup", systemImage: "bolt.circle") {
                            openQuickCapture()
                        }
                        .buttonStyle(.bordered)
                    }
                }
                .id(HomeSection.capture.id)

                SectionCard(title: "Return to a Thread", systemImage: "arrow.clockwise.circle") {
                    if store.homeThreads.isEmpty {
                        Text("No thread yet. Start with a capture or create one from a note.")
                            .foregroundStyle(.secondary)
                    } else {
                        VStack(spacing: 10) {
                            ForEach(Array(store.homeThreads.prefix(4))) { thread in
                                Button {
                                    store.openThread(thread.id)
                                } label: {
                                    VStack(alignment: .leading, spacing: 6) {
                                        HStack {
                                            Text(thread.title)
                                                .font(.headline)
                                            Spacer()
                                            Text(thread.lastActiveAt, style: .relative)
                                                .font(.caption)
                                                .foregroundStyle(.secondary)
                                        }
                                        Text(thread.summary)
                                            .foregroundStyle(.secondary)
                                            .lineLimit(2)
                                        Text("Next: \(thread.nextStep)")
                                            .font(.caption)
                                            .foregroundStyle(.tertiary)
                                    }
                                    .frame(maxWidth: .infinity, alignment: .leading)
                                    .padding(14)
                                    .background(.thinMaterial, in: .rect(cornerRadius: 16))
                                }
                                .buttonStyle(.plain)
                            }
                        }
                    }
                }
                .id(HomeSection.resume.id)

                if !store.homeLists.isEmpty {
                    SectionCard(title: "Lists", systemImage: "rectangle.stack") {
                        Text("Lists are lightweight packs. They collect notes and threads without turning into a new problem space.")
                            .font(.caption)
                            .foregroundStyle(.secondary)

                        VStack(spacing: 10) {
                            ForEach(Array(store.homeLists.prefix(3))) { list in
                                Button {
                                    store.openList(list.id)
                                } label: {
                                    VStack(alignment: .leading, spacing: 6) {
                                        HStack {
                                            Text(list.title)
                                                .font(.headline)
                                            Spacer()
                                            Text(list.kind.title)
                                                .font(.caption)
                                                .foregroundStyle(.secondary)
                                        }
                                        Text(list.note)
                                            .foregroundStyle(.secondary)
                                            .lineLimit(2)
                                        Text("\(store.threadCount(for: list.id)) thread(s) · \(store.entryCount(for: list.id)) note(s)")
                                            .font(.caption)
                                            .foregroundStyle(.tertiary)
                                    }
                                    .frame(maxWidth: .infinity, alignment: .leading)
                                    .padding(14)
                                    .background(.thinMaterial, in: .rect(cornerRadius: 16))
                                }
                                .buttonStyle(.plain)
                            }
                        }
                    }
                    .id(HomeSection.lists.id)
                }

                if !store.needsSortingEntries.isEmpty {
                    SectionCard(title: "Needs Sorting", systemImage: "tray") {
                        VStack(spacing: 12) {
                            ForEach(store.needsSortingEntries) { entry in
                                HomeNoteCard(
                                    entry: entry,
                                    primaryThread: store.primaryThread(for: entry),
                                    additionalThreadCount: store.secondaryThreadCount(for: entry),
                                    suggestions: store.suggestedThreads(for: entry),
                                    onOpenThread: { threadID in
                                        store.openThreadFromNote(threadID: threadID, entryID: entry.id)
                                    },
                                    onResolveSuggestion: { threadID in
                                        store.resolveInboxEntry(entry, to: threadID)
                                    },
                                    onCreateThread: {
                                        store.createThreadLinkingExistingEntry(entry.id)
                                    }
                                )
                            }
                        }
                    }
                    .id(HomeSection.sorting.id)
                }

                SectionCard(title: "Recent Notes", systemImage: "note.text") {
                    if store.homeRecentEntries.isEmpty {
                        Text("Your recent captures will stay visible here.")
                            .foregroundStyle(.secondary)
                    } else {
                        VStack(spacing: 12) {
                            ForEach(store.homeRecentEntries) { entry in
                                HomeNoteCard(
                                    entry: entry,
                                    primaryThread: store.primaryThread(for: entry),
                                    additionalThreadCount: store.secondaryThreadCount(for: entry),
                                    suggestions: [],
                                    onOpenThread: { threadID in
                                        store.openThreadFromNote(threadID: threadID, entryID: entry.id)
                                    },
                                    onResolveSuggestion: { _ in },
                                    onCreateThread: {
                                        store.createThreadLinkingExistingEntry(entry.id)
                                    }
                                )
                            }
                        }
                    }
                }
                .id(HomeSection.recent.id)
            }
            .frame(maxWidth: 860, alignment: .leading)
            .frame(maxWidth: .infinity)
            .padding(.bottom, 6)
        }
        .scrollIndicators(.hidden)
    }
}

private struct ThreadPageView: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord

    var body: some View {
        let threadState = store.threadState(for: thread.id)
        let sourceEntry = store.selectedEntry
        let sourceEntryBelongsToThread = sourceEntry.map { store.isEntry($0.id, inThread: thread.id) } ?? false
        let sourceEntryInRecentSessions = sourceEntry.map { entry in
            threadState?.recentSessions.contains(where: { session in
                session.entries.contains(where: { $0.id == entry.id })
            }) ?? false
        } ?? false

        HSplitView {
            ThreadSidebarView(store: store)
                .frame(minWidth: 240, idealWidth: 260, maxWidth: 300)

            ScrollView {
                VStack(alignment: .leading, spacing: 14) {
                    if let sourceEntry, sourceEntryBelongsToThread {
                        OpenedFromNoteCard(entry: sourceEntry)
                    }

                    if let threadState {
                        WorkingStreamCard(
                            sessions: threadState.recentSessions,
                            lastAnchorAt: threadState.lastAnchorAt
                        )
                    }

                    ContinueComposerCard(
                        draft: $store.inlineNoteDraft,
                        onSubmit: {
                            store.appendInlineNote(to: thread.id)
                        }
                    )
                }
                .frame(maxWidth: 720, alignment: .leading)
                .frame(maxWidth: .infinity)
                .padding(.bottom, 6)
            }
            .scrollIndicators(.hidden)

            ScrollView {
                VStack(alignment: .leading, spacing: 14) {
                    ProblemHeaderCard(store: store, thread: thread, threadState: threadState)

                    if let threadState {
                        ResumeStripCard(threadState: threadState)

                        if let sourceEntry, sourceEntryBelongsToThread, !sourceEntryInRecentSessions {
                            SourceNoteCard(entry: sourceEntry)
                        }

                        RelatedNotesCard(
                            entries: threadState.relatedEntries,
                            highlightedEntryID: sourceEntryBelongsToThread ? sourceEntry?.id : nil
                        )
                    }
                }
                .padding(.bottom, 6)
            }
            .frame(minWidth: 320, idealWidth: 360, maxWidth: 420)
            .scrollIndicators(.hidden)
        }
    }
}

private struct ThreadSidebarView: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        SidebarSurface {
            VStack(alignment: .leading, spacing: 18) {
                Button("Back to Home", systemImage: "chevron.left") {
                    store.goHome()
                }
                .buttonStyle(.borderless)

                VStack(alignment: .leading, spacing: 4) {
                    Text("Threads")
                        .font(.headline)
                    Text("Use the left column to jump between problem spaces without leaving the current layout.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }

                ScrollView {
                    VStack(spacing: 8) {
                        ForEach(store.homeThreads) { thread in
                            Button {
                                store.openThread(thread.id)
                            } label: {
                                VStack(alignment: .leading, spacing: 4) {
                                    Text(thread.title)
                                        .font(.subheadline.weight(.semibold))
                                        .foregroundStyle(.primary)
                                        .lineLimit(2)
                                    Text(thread.nextStep)
                                        .font(.caption)
                                        .foregroundStyle(.secondary)
                                        .lineLimit(2)
                                }
                                .frame(maxWidth: .infinity, alignment: .leading)
                                .padding(12)
                                .background(
                                    thread.id == store.selectedThreadID
                                        ? Color.accentColor.opacity(0.16)
                                        : Color.clear,
                                    in: .rect(cornerRadius: 14)
                                )
                            }
                            .buttonStyle(.plain)
                        }
                    }
                    .padding(.vertical, 2)
                }
                .scrollIndicators(.hidden)

                Spacer()
            }
        }
    }
}

private struct OpenedFromNoteCard: View {
    let entry: Entry

    var body: some View {
        SectionCard(title: "Opened from Note", systemImage: "arrow.turn.down.right") {
            Text(entry.content)
                .font(.body)
            HStack {
                Text(entry.kind.rawValue.capitalized)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }
        }
    }
}

private struct ProblemHeaderCard: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord
    let threadState: ThreadState?

    var body: some View {
        SectionCard(title: "Problem", systemImage: "square.stack.3d.up") {
            Text(thread.title)
                .font(.title2.bold())

            Text(thread.prompt)
                .font(.body)
                .foregroundStyle(.secondary)

            HStack {
                Label(thread.status.rawValue.capitalized, systemImage: "circle.fill")
                    .font(.caption)
                    .foregroundStyle(.secondary)

                Spacer()

                Text("Last active \(thread.lastActiveAt.formatted(.relative(presentation: .named)))")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            HStack {
                Button("Prepare Draft", systemImage: "square.and.pencil") {
                    store.prepareView(type: .writing, for: thread.id)
                }
                .buttonStyle(.borderedProminent)

                Spacer()

                if let nextAction = threadState?.nextAction {
                    Text("Next: \(nextAction)")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                } else {
                    Text("Progress is remembered automatically when you leave.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }

            Divider()

            if let threadState, !threadState.keyClaims.isEmpty {
                Text("Current Claims")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                ForEach(threadState.keyClaims) { claim in
                    Text("• \(claim.statement)")
                        .font(.subheadline)
                }
            } else if let threadState {
                Text(threadState.currentJudgment)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .lineLimit(3)
            } else {
                Text(thread.summary)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .lineLimit(3)
            }
        }
    }
}

private struct ResumeStripCard: View {
    let threadState: ThreadState

    var body: some View {
        SectionCard(title: "Resume Here", systemImage: "arrow.uturn.backward.circle") {
            Text(threadState.currentJudgment)
                .font(.headline)

            if !threadState.openLoops.isEmpty {
                Divider()
                Text("Still Open")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                ForEach(Array(threadState.openLoops.prefix(2)), id: \.self) { loop in
                    Text("• \(loop)")
                }
            }

            if let nextMove = threadState.nextAction {
                Divider()
                Text("Next Action")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(nextMove)
            }
        }
    }
}

private struct WorkingStreamCard: View {
    let sessions: [ThreadSession]
    let lastAnchorAt: Date?

    var body: some View {
        SectionCard(title: "Working Stream", systemImage: "clock.arrow.circlepath") {
            if let lastAnchorAt {
                Text("Since the last remembered point from \(lastAnchorAt.formatted(.relative(presentation: .named)))")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            } else {
                Text("Recent changes in this problem space.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            if sessions.isEmpty {
                Text("No visible changes since the last remembered state.")
                    .foregroundStyle(.secondary)
            } else {
                VStack(spacing: 12) {
                    ForEach(sessions) { session in
                        VStack(alignment: .leading, spacing: 6) {
                            HStack {
                                Text(sessionTitle(session))
                                    .font(.caption.weight(.semibold))
                                    .foregroundStyle(.secondary)
                                Spacer()
                                Text(session.endedAt, style: .relative)
                                    .font(.caption)
                                    .foregroundStyle(.tertiary)
                            }

                            ForEach(session.entries) { entry in
                                VStack(alignment: .leading, spacing: 4) {
                                    Text(entry.content)
                                        .font(.body)
                                    Text(entry.kind.rawValue.capitalized)
                                        .font(.caption)
                                        .foregroundStyle(.secondary)
                                }
                                .frame(maxWidth: .infinity, alignment: .leading)

                                if entry.id != session.entries.last?.id {
                                    Divider()
                                }
                            }
                        }
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .padding(14)
                        .background(.thinMaterial, in: .rect(cornerRadius: 16))
                    }
                }
            }
        }
    }

    private func sessionTitle(_ session: ThreadSession) -> String {
        if session.entries.count == 1 {
            return "Single step"
        }
        return "Work session"
    }
}

private struct SourceNoteCard: View {
    let entry: Entry

    var body: some View {
        SectionCard(title: "Source Note", systemImage: "pin") {
            Text("This note brought you into the thread, but it sits outside the latest working sessions.")
                .font(.caption)
                .foregroundStyle(.secondary)

            Divider()

            Text(entry.content)
                .font(.body)

            HStack {
                Text(entry.kind.rawValue.capitalized)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }
        }
    }
}

private struct RelatedNotesCard: View {
    let entries: [Entry]
    let highlightedEntryID: UUID?

    var body: some View {
        let rows = entries.map { entry in
            RelatedNoteRow(
                id: entry.id,
                entry: entry,
                isHighlighted: entry.id == highlightedEntryID
            )
        }

        SectionCard(title: "Related Notes", systemImage: "link") {
            if rows.isEmpty {
                Text("No related notes yet.")
                    .foregroundStyle(.secondary)
            } else {
                VStack(spacing: 10) {
                    SwiftUI.ForEach(rows, id: \.id) { (row: RelatedNoteRow) in
                        VStack(alignment: .leading, spacing: 6) {
                            if row.isHighlighted {
                                Text("Opened from this note")
                                    .font(.caption.weight(.semibold))
                                    .foregroundStyle(Color.accentColor)
                            }

                            Text(row.entry.content)
                                .font(.body)

                            HStack {
                                Text(row.entry.kind.rawValue.capitalized)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                                Spacer()
                                Text(row.entry.createdAt, style: .relative)
                                    .font(.caption)
                                    .foregroundStyle(.tertiary)
                            }
                        }
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .padding(14)
                        .background(.thinMaterial, in: .rect(cornerRadius: 16))
                    }
                }
            }
        }
    }
}

private struct RelatedNoteRow: Identifiable {
    let id: UUID
    let entry: Entry
    let isHighlighted: Bool
}

private struct ContinueComposerCard: View {
    @Binding var draft: String
    let onSubmit: () -> Void

    var body: some View {
        SectionCard(title: "Continue", systemImage: "text.cursor") {
            TextEditor(text: $draft)
                .font(.body)
                .scrollContentBackground(.hidden)
                .frame(minHeight: 120)
                .padding(10)
                .background(.thinMaterial, in: .rect(cornerRadius: 14))

            HStack {
                Text("Keep the thread moving from here.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Button("Add Note") {
                    onSubmit()
                }
                .buttonStyle(.borderedProminent)
            }
        }
    }
}

private struct DraftSheetView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Draft")
                        .font(.title2.bold())
                    Text("A task-specific view generated from the current thread.")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Button("Close", systemImage: "xmark") {
                    store.preparedView = nil
                    dismiss()
                }
                .buttonStyle(.borderless)
            }

            if store.preparedView != nil {
                Picker("Draft Type", selection: preparedViewTypeBinding) {
                    ForEach(PreparedViewType.allCases) { type in
                        Text(type.title).tag(type)
                    }
                }
                .pickerStyle(.segmented)

                if let preparedView = store.preparedView {
                    PreparedViewCard(preparedView: preparedView)
                }
            }
        }
        .padding(20)
        .frame(minWidth: 620, minHeight: 520)
    }

    private var preparedViewTypeBinding: Binding<PreparedViewType> {
        Binding(
            get: { store.preparedViewType },
            set: { store.refreshPreparedView(as: $0) }
        )
    }
}

private struct ListSheetView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text(store.presentedList?.title ?? "List")
                        .font(.title2.bold())
                    Text(store.presentedList?.note ?? "A lightweight pack for grouping notes and threads.")
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Button("Close", systemImage: "xmark") {
                    store.presentedListID = nil
                    dismiss()
                }
                .buttonStyle(.borderless)
            }

            if let list = store.presentedList {
                Text("Lists collect things for a purpose. They do not carry current state or replace threads.")
                    .font(.caption)
                    .foregroundStyle(.secondary)

                ScrollView {
                    VStack(alignment: .leading, spacing: 12) {
                        ForEach(store.items(for: list.id)) { item in
                            if let thread = store.listThread(item) {
                                Button {
                                    store.presentedListID = nil
                                    dismiss()
                                    store.openThread(thread.id)
                                } label: {
                                    VStack(alignment: .leading, spacing: 6) {
                                        Label(thread.title, systemImage: "square.stack.3d.up")
                                            .font(.headline)
                                            .frame(maxWidth: .infinity, alignment: .leading)
                                        Text(thread.summary)
                                            .foregroundStyle(.secondary)
                                            .lineLimit(2)
                                        if let note = item.note {
                                            Text(note)
                                                .font(.caption)
                                                .foregroundStyle(.tertiary)
                                        }
                                    }
                                    .padding(14)
                                    .frame(maxWidth: .infinity, alignment: .leading)
                                    .background(.thinMaterial, in: .rect(cornerRadius: 16))
                                }
                                .buttonStyle(.plain)
                            } else if let entry = store.listEntry(item) {
                                VStack(alignment: .leading, spacing: 6) {
                                    Label(entry.kind.rawValue.capitalized, systemImage: "note.text")
                                        .font(.headline)
                                    Text(entry.content)
                                        .font(.body)
                                    if let note = item.note {
                                        Text(note)
                                            .font(.caption)
                                            .foregroundStyle(.secondary)
                                    }
                                }
                                .padding(14)
                                .frame(maxWidth: .infinity, alignment: .leading)
                                .background(.thinMaterial, in: .rect(cornerRadius: 16))
                            }
                        }
                    }
                    .padding(.bottom, 6)
                }
            }
        }
        .padding(20)
        .frame(minWidth: 560, minHeight: 420)
    }
}

private struct HomeNoteCard: View {
    let entry: Entry
    let primaryThread: ThreadRecord?
    let additionalThreadCount: Int
    let suggestions: [ThreadSuggestion]
    let onOpenThread: (UUID) -> Void
    let onResolveSuggestion: (UUID) -> Void
    let onCreateThread: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                Text(entry.kind.rawValue)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }

            Text(entry.content)
                .font(.body)

            if let primaryThread {
                Button {
                    onOpenThread(primaryThread.id)
                } label: {
                    Label("In \(primaryThread.title)", systemImage: "arrow.turn.down.right")
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
                .buttonStyle(.bordered)

                if additionalThreadCount > 0 {
                    Text("Also related to \(additionalThreadCount) more thread(s).")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            } else {
                HStack(spacing: 6) {
                    Image(systemName: "tray")
                    Text("Needs thread")
                }
                .font(.caption.weight(.semibold))
                .foregroundStyle(.orange)
            }

            if !suggestions.isEmpty {
                VStack(alignment: .leading, spacing: 6) {
                    Text("Suggested Threads")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)

                    ForEach(suggestions) { suggestion in
                        Button {
                            onResolveSuggestion(suggestion.thread.id)
                        } label: {
                            VStack(alignment: .leading, spacing: 3) {
                                Label(suggestion.thread.title, systemImage: "sparkles")
                                    .frame(maxWidth: .infinity, alignment: .leading)
                                Text(suggestion.reason)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                            }
                        }
                        .buttonStyle(.bordered)
                    }
                }
            }

            if primaryThread == nil {
                Button("Create Thread", systemImage: "square.stack.badge.plus") {
                    onCreateThread()
                }
                .buttonStyle(.borderedProminent)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(16)
        .background(.thinMaterial, in: .rect(cornerRadius: 18))
    }
}

private struct PreparedViewCard: View {
    let preparedView: PreparedView

    var body: some View {
        SectionCard(title: preparedView.title, systemImage: "square.and.pencil") {
            Text(preparedView.coreQuestion)
                .font(.headline)

            Divider()

            Text("Claims")
                .font(.subheadline.weight(.semibold))
            if preparedView.activeClaims.isEmpty {
                Text("No active claims yet.")
                    .foregroundStyle(.secondary)
            } else {
                ForEach(preparedView.activeClaims) { claim in
                    Text("• \(claim.statement)")
                }
            }

            Divider()

            Text("Evidence")
                .font(.subheadline.weight(.semibold))
            if preparedView.keyEvidence.isEmpty {
                Text("No key evidence selected yet.")
                    .foregroundStyle(.secondary)
            } else {
                ForEach(preparedView.keyEvidence) { entry in
                    Text("• \(entry.content)")
                }
            }

            Divider()

            Text("Next Steps")
                .font(.subheadline.weight(.semibold))
            if preparedView.recommendedNextSteps.isEmpty {
                Text("No next step suggested yet.")
                    .foregroundStyle(.secondary)
            } else {
                ForEach(preparedView.recommendedNextSteps, id: \.self) { step in
                    Text("• \(step)")
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

private struct SectionCard<Content: View>: View {
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
        .background(.regularMaterial, in: .rect(cornerRadius: 20))
    }
}

private struct SidebarSurface<Content: View>: View {
    @ViewBuilder let content: Content

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            content
        }
        .padding(16)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .background(.thickMaterial, in: .rect(cornerRadius: 24))
    }
}
