import AppKit
import SwiftUI

struct ContentView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.colorScheme) private var colorScheme

    var body: some View {
        VStack(spacing: 0) {
            header
            Divider()

            HSplitView {
                WorkspaceSidebar(store: store)
                    .frame(minWidth: 220, idealWidth: 260, maxWidth: 320)

                WorkspaceCanvasSurface {
                    currentCanvas
                }
                .frame(minWidth: 560, maxWidth: .infinity, maxHeight: .infinity)

                WorkspaceInspector(store: store)
                    .frame(minWidth: 240, idealWidth: 290, maxWidth: 360)
            }
            .padding(14)
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
        .onChange(of: scenePhase) { _, newPhase in
            if newPhase != .active {
                store.maybeWriteAnchorIfNeeded(for: store.selectedThreadID)
            }
        }
        .sheet(item: $store.threadCreationContext) { context in
            NewThreadSheet(store: store, context: context)
                .frame(minWidth: 460, minHeight: 420)
        }
        .sheet(isPresented: sourceSheetBinding) {
            if let entry = store.selectedSourceEntry {
                SourceDetailSheet(store: store, entry: entry)
                    .frame(minWidth: 520, minHeight: 420)
            }
        }
        .sheet(item: resourcesThreadBinding) { thread in
            ThreadResourcesSheet(store: store, thread: thread)
                .frame(minWidth: 560, minHeight: 520)
        }
    }

    private var header: some View {
        HStack(spacing: 16) {
            VStack(alignment: .leading, spacing: 2) {
                Text(headerTitle)
                    .font(.title2.bold())
                Text(headerSubtitle)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            Spacer()
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 14)
        .background(.bar)
    }

    private var headerTitle: String {
        if store.isInWorkbench {
            return store.selectedThread?.title ?? "Thread"
        }
        if let list = store.selectedList {
            return list.title
        }
        return "Inbox"
    }

    private var headerSubtitle: String {
        if store.isInWorkbench {
            return store.selectedThread.map(threadSubtitle(for:)) ?? "Resume one problem at a time."
        }
        if let list = store.selectedList {
            let count = store.items(for: list.id).count
            return "\(list.kind.title) • \(count) \(count == 1 ? "item" : "items")"
        }
        return "Capture first. Sort it later."
    }

    @ViewBuilder
    private var currentCanvas: some View {
        if store.isInWorkbench, let thread = store.selectedThread {
            WorkbenchView(store: store, thread: thread)
        } else if let list = store.selectedList {
            ListDetailView(store: store, list: list)
        } else {
            StreamContentView(store: store)
        }
    }

    private var sourceSheetBinding: Binding<Bool> {
        Binding(
            get: { store.selectedSourceEntry != nil },
            set: { isPresented in
                if !isPresented {
                    store.closeSource()
                }
            }
        )
    }

    private var resourcesThreadBinding: Binding<ThreadRecord?> {
        Binding(
            get: { store.selectedResourcesThread },
            set: { newValue in
                if let newValue {
                    store.selectedResourcesThreadID = newValue.id
                } else {
                    store.closeThreadResources()
                }
            }
        )
    }
}

// MARK: - Stream

private enum ListDetailFilter: String, CaseIterable, Identifiable {
    case all
    case threads
    case notes
    case sources

    var id: Self { self }

    var title: String {
        switch self {
        case .all:
            "All"
        case .threads:
            "Threads"
        case .notes:
            "Notes"
        case .sources:
            "Sources"
        }
    }

    func matches(_ type: ListItemType) -> Bool {
        switch self {
        case .all:
            true
        case .threads:
            type == .thread
        case .notes:
            type == .entry
        case .sources:
            type == .source
        }
    }
}

private enum ListSortMode: String, CaseIterable, Identifiable {
    case saved
    case recentAdded
    case recentActive

    var id: Self { self }

    var title: String {
        switch self {
        case .saved:
            "Saved"
        case .recentAdded:
            "Recent"
        case .recentActive:
            "Active"
        }
    }
}

private struct ListResolvedItem: Identifiable {
    let listItem: ListItem
    let thread: ThreadRecord?
    let entry: Entry?

    var id: UUID { listItem.id }
}

private struct NewThreadSheet: View {
    @Bindable var store: ThreadnoteStore
    let context: ThreadCreationContext

    @State private var title: String
    @State private var goalType: ThreadGoalType

    init(store: ThreadnoteStore, context: ThreadCreationContext) {
        self.store = store
        self.context = context
        _title = State(initialValue: context.suggestedTitle)
        _goalType = State(initialValue: context.suggestedGoalType)
    }

    private var trimmedTitle: String {
        title.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 18) {
            Text(context.editingThreadID == nil ? "New Thread" : "Edit Thread")
                .font(.title2.bold())

            Text("Name the thread, then choose the kind of work it belongs to. The title now carries the intent; the category keeps Resume shaped correctly.")
                .font(.subheadline)
                .foregroundStyle(.secondary)

            VStack(alignment: .leading, spacing: 12) {
                Text("Title")
                    .font(.headline)
                TextField("Short thread title", text: $title)
                    .textFieldStyle(.roundedBorder)

                Text("Category")
                    .font(.headline)
                Picker("Category", selection: $goalType) {
                    ForEach(ThreadGoalType.allCases) { option in
                        Label(option.title, systemImage: option.systemImage)
                            .tag(option)
                    }
                }
                .pickerStyle(.segmented)
            }

            Spacer()

            HStack {
                Spacer()
                Button("Cancel") {
                    store.dismissThreadCreation()
                }
                .buttonStyle(.bordered)

                Button(context.editingThreadID == nil ? "Create Thread" : "Save Thread") {
                    store.completeThreadCreation(
                        title: title,
                        goalType: goalType
                    )
                }
                .buttonStyle(.borderedProminent)
                .disabled(trimmedTitle.isEmpty)
            }
        }
        .padding(24)
        .background(Color(nsColor: .windowBackgroundColor))
    }
}

private struct WorkspaceSidebar: View {
    @Bindable var store: ThreadnoteStore
    @State private var showingNewListSheet = false

    var body: some View {
        SidebarSurface {
            ScrollView {
                VStack(alignment: .leading, spacing: 18) {
                    VStack(alignment: .leading, spacing: 8) {
                        StreamSidebarRow(store: store)
                    }

                    VStack(alignment: .leading, spacing: 8) {
                        HStack {
                            Text("Threads")
                                .font(.headline)
                            Spacer()
                            Button {
                                store.beginThreadCreation()
                            } label: {
                                Label("New Thread", systemImage: "plus")
                                    .labelStyle(.iconOnly)
                            }
                            .buttonStyle(.borderless)
                            .help("Create a new thread with a goal")
                        }

                        VStack(alignment: .leading, spacing: 6) {
                            ForEach(store.homeThreads) { thread in
                                ThreadSidebarRow(store: store, thread: thread)
                            }
                        }
                    }

                    VStack(alignment: .leading, spacing: 8) {
                        HStack {
                            Text("Lists")
                                .font(.headline)
                            Spacer()
                            Button {
                                showingNewListSheet = true
                            } label: {
                                Label("New List", systemImage: "plus")
                                    .labelStyle(.iconOnly)
                            }
                            .buttonStyle(.borderless)
                            .help("Create a new resource list")
                        }

                        if store.lists.isEmpty {
                            Text("No lists yet.")
                                .font(.caption)
                                .foregroundStyle(.secondary)
                        } else {
                            VStack(alignment: .leading, spacing: 6) {
                                ForEach(store.lists.sorted { $0.updatedAt > $1.updatedAt }) { list in
                                    ListSidebarRow(store: store, list: list)
                                }
                            }
                        }
                    }
                }
            }
        }
        .sheet(isPresented: $showingNewListSheet) {
            NewListSheet(store: store, isPresented: $showingNewListSheet)
                .frame(minWidth: 420, minHeight: 320)
        }
    }
}

private struct WorkspaceCanvasSurface<Content: View>: View {
    @ViewBuilder let content: Content

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            content
        }
        .padding(18)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .background(.regularMaterial, in: .rect(cornerRadius: 24))
    }
}

private struct WorkspaceInspector: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        SidebarSurface {
            ScrollView {
                VStack(alignment: .leading, spacing: 12) {
                    if let thread = store.selectedThread {
                        threadInspector(thread)
                    } else if let list = store.selectedList {
                        listInspector(list)
                    } else {
                        streamInspector
                    }
                }
            }
        }
    }

    @ViewBuilder
    private func threadInspector(_ thread: ThreadRecord) -> some View {
        if let state = store.threadState(for: thread.id) {
            ThreadMemoryRail(thread: thread, state: state)
        }
    }

    @ViewBuilder
    private func listInspector(_ list: ListRecord) -> some View {
        SectionCard(title: "Resource View", systemImage: listKindSymbol(for: list.kind)) {
            Text(list.title)
                .font(.headline)
            if !list.description.isEmpty {
                Text(list.description)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }
            let count = store.items(for: list.id).count
            Text("\(list.kind.title) · \(count) \(count == 1 ? "item" : "items")")
                .font(.caption)
                .foregroundStyle(.secondary)
        }

        SectionCard(title: "Workspace Guidance", systemImage: "lightbulb") {
            Text("Keep this list as a resource lens. Add threads, notes, and sources from context menus in stream or thread views.")
                .font(.caption)
                .foregroundStyle(.secondary)
        }
    }

    private var streamInspector: some View {
        VStack(alignment: .leading, spacing: 12) {
            SectionCard(title: "Inbox", systemImage: "tray.full") {
                Text("Drop notes here first. Threads and lists stay focused on what happens next.")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            SectionCard(title: "Workspace Snapshot", systemImage: "square.grid.2x2") {
                Text(inboxStatusLine)
                    .font(.callout)
                Text("\(store.homeThreads.count) threads · \(store.lists.count) lists")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }

    private var inboxStatusLine: String {
        let count = store.inboxEntries.count
        return count == 1 ? "1 note waiting" : "\(count) notes waiting"
    }
}

private let workspaceCanvasContentMaxWidth: CGFloat = 980

private extension View {
    func workspaceCanvasColumn() -> some View {
        frame(maxWidth: workspaceCanvasContentMaxWidth, alignment: .leading)
            .frame(maxWidth: .infinity, alignment: .center)
    }
}

private struct StreamSidebarRow: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        Button {
            store.goToStream()
        } label: {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 4) {
                    Label("Inbox", systemImage: "tray.full")
                        .font(.subheadline.weight(.medium))
                        .lineLimit(2)
                    Text(streamSubtitle)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                if store.inboxEntries.count > 0 {
                    Text("\(store.inboxEntries.count)")
                        .font(.caption2.weight(.bold))
                        .foregroundStyle(.orange)
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(Color.orange.opacity(0.12), in: .capsule)
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 10)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.thinMaterial, in: .rect(cornerRadius: 12))
            .overlay {
                RoundedRectangle(cornerRadius: 12)
                    .stroke(isSelected ? Color.accentColor.opacity(0.35) : Color.clear, lineWidth: 1)
            }
        }
        .buttonStyle(.plain)
    }

    private var isSelected: Bool {
        !store.isInWorkbench && store.selectedList == nil
    }

    private var streamSubtitle: String {
        if store.inboxEntries.isEmpty {
            return "Capture first. Sort it later."
        }
        return "\(store.inboxEntries.count) waiting"
    }
}

private struct StreamContentView: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        VStack(spacing: 0) {
            StreamInputBox(store: store)
                .workspaceCanvasColumn()

            Divider()
                .padding(.vertical, 8)

            ScrollView {
                LazyVStack(alignment: .leading, spacing: 10) {
                    ForEach(store.streamEntries) { entry in
                        StreamEntryRow(store: store, entry: entry)
                    }
                }
                .workspaceCanvasColumn()
            }
        }
    }
}

private struct ListDetailView: View {
    @Bindable var store: ThreadnoteStore
    let list: ListRecord
    @State private var filter: ListDetailFilter = .all
    @State private var sortMode: ListSortMode = .saved
    private let shelfColumns = [GridItem(.adaptive(minimum: 280), spacing: 14)]

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .center, spacing: 16) {
                    VStack(alignment: .leading, spacing: 6) {
                        Label("\(list.kind.title) • \(resolvedItems.count) saved \(resolvedItems.count == 1 ? "resource" : "resources")", systemImage: listKindSymbol(for: list.kind))
                            .font(.caption.weight(.semibold))
                            .font(.caption)
                            .foregroundStyle(.secondary)
                        Text(list.title)
                            .font(.title3.bold())
                    }

                    Spacer()

                    HStack(spacing: 12) {
                        Picker("Filter", selection: $filter) {
                            ForEach(ListDetailFilter.allCases) { option in
                                Text(option.title).tag(option)
                            }
                        }
                        .pickerStyle(.segmented)
                        .frame(maxWidth: 360)

                        Menu {
                            Picker("Sort", selection: $sortMode) {
                                ForEach(ListSortMode.allCases) { option in
                                    Text(option.title).tag(option)
                                }
                            }
                        } label: {
                            Label(sortMode.title, systemImage: "arrow.up.arrow.down")
                        }
                    }
                }

                if !list.description.isEmpty {
                    Text(list.description)
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                }
            }
            .workspaceCanvasColumn()

            Divider()
                .padding(.vertical, 8)

            ScrollView {
                LazyVStack(alignment: .leading, spacing: 18) {
                    if filteredItems.isEmpty {
                        ContentUnavailableView(
                            "No \(filter.title.lowercased()) here",
                            systemImage: "tray",
                            description: Text(emptyStateDescription)
                        )
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 48)
                    } else {
                        ResourceOverviewRow(items: filteredItems)

                        if !pinnedItems.isEmpty {
                            ListSectionCard(title: "Pinned", subtitle: "Keep the most important resources surfaced at the top of this view.", systemImage: "pin") {
                                LazyVGrid(columns: shelfColumns, alignment: .leading, spacing: 14) {
                                    ForEach(pinnedItems) { item in
                                        ListItemCard(store: store, item: item)
                                    }
                                }
                            }
                        }

                        if !focusThreadItems.isEmpty {
                            ListSectionCard(title: "Thread Focus", subtitle: "Problem spaces this resource view is orbiting around.", systemImage: "square.stack.3d.up") {
                                ScrollView(.horizontal) {
                                    HStack(spacing: 14) {
                                        ForEach(focusThreadItems) { item in
                                            ListFocusThreadCard(store: store, item: item)
                                                .frame(width: 320)
                                        }
                                    }
                                }
                                .scrollIndicators(.hidden)
                            }
                        }

                        if !shelfItems.isEmpty {
                            ListSectionCard(title: shelfTitle, subtitle: shelfSubtitle, systemImage: shelfSystemImage) {
                                LazyVGrid(columns: shelfColumns, alignment: .leading, spacing: 14) {
                                    ForEach(shelfItems) { item in
                                        ListItemCard(store: store, item: item)
                                    }
                                }
                            }
                        }
                    }
                }
                .workspaceCanvasColumn()
            }
        }
    }

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

    private var pinnedItems: [ListResolvedItem] {
        filteredItems.filter(\.listItem.isPinned)
    }

    private var unpinnedItems: [ListResolvedItem] {
        filteredItems.filter { !$0.listItem.isPinned }
    }

    private var focusThreadItems: [ListResolvedItem] {
        guard filter == .all || filter == .threads else { return [] }
        return unpinnedItems.filter { $0.listItem.itemType == .thread }
    }

    private var shelfItems: [ListResolvedItem] {
        switch filter {
        case .all:
            return unpinnedItems.filter { $0.listItem.itemType != .thread }
        case .threads:
            return []
        case .notes, .sources:
            return unpinnedItems
        }
    }

    private var shelfTitle: String {
        switch filter {
        case .all:
            "Resource Shelf"
        case .notes:
            "Notes"
        case .sources:
            "Sources"
        case .threads:
            "Threads"
        }
    }

    private var shelfSubtitle: String {
        switch filter {
        case .all:
            "Collect notes and sources here without turning the list into another problem space."
        case .notes:
            "Saved notes and observations in this view."
        case .sources:
            "References and citations gathered in this view."
        case .threads:
            "Problem spaces collected here."
        }
    }

    private var shelfSystemImage: String {
        switch filter {
        case .all:
            "square.grid.2x2"
        case .notes:
            "note.text"
        case .sources:
            "bookmark"
        case .threads:
            "square.stack.3d.up"
        }
    }

    private var emptyStateDescription: String {
        if filter == .all {
            return "This list is empty. Add threads, notes, or sources from the stream context menus."
        }
        return "Switch filters or add more resources from a thread or note context menu."
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
            return items.sorted { lhs, rhs in
                sortDate(for: lhs) > sortDate(for: rhs)
            }
        }
    }

    private func sortDate(for item: ListResolvedItem) -> Date {
        if let thread = item.thread {
            return thread.lastActiveAt
        }
        if let entry = item.entry, let thread = store.thread(for: entry) {
            return max(entry.createdAt, thread.lastActiveAt)
        }
        return item.listItem.addedAt
    }
}

private struct StreamInputBox: View {
    @Bindable var store: ThreadnoteStore
    @State private var draft = ""

    var body: some View {
        CaptureComposer(
            text: $draft,
            helperText: "Use `#role` to classify the note, for example `#question`, `#claim`, `#evidence`, or `#decided`.",
            submitLabel: "Send",
            minHeight: 92
        ) {
            submitStreamCapture()
        }
        .frame(minHeight: 92, maxHeight: 156)
        .padding(.bottom, 4)
    }

    private func submitStreamCapture() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        store.quickCaptureDraft = QuickCaptureDraft(text: text)
        store.submitCapture()
        draft = ""
    }
}

private struct StreamEntryRow: View {
    @Bindable var store: ThreadnoteStore
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                EntryKindBadge(kind: entry.kind)
                Spacer()
                Text(entry.createdAt, format: .dateTime.year().month().day())
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }

            EntryBodyView(entry: entry)

            if entry.isSourceResource {
                Button("View Source", systemImage: "bookmark") {
                    store.openSource(entry.id)
                }
                .buttonStyle(.borderedProminent)
            }

            if let thread = store.thread(for: entry) {
                Button {
                    store.openThread(thread.id)
                } label: {
                    Label("In \(thread.title)", systemImage: "arrow.turn.down.right")
                        .font(.caption)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
                .buttonStyle(.bordered)
            } else {
                HStack(spacing: 6) {
                    Image(systemName: "tray")
                    Text("Unrouted")
                }
                .font(.caption.weight(.semibold))
                .foregroundStyle(.orange)

                let suggestions = store.suggestedThreads(for: entry, limit: 2)
                if !suggestions.isEmpty {
                    ForEach(suggestions) { suggestion in
                        Button {
                            store.resolveInboxEntry(entry, to: suggestion.thread.id)
                        } label: {
                            VStack(alignment: .leading, spacing: 2) {
                                Label(suggestion.thread.title, systemImage: "sparkles")
                                    .font(.caption.weight(.semibold))
                                Text(suggestion.reason)
                                    .font(.caption2)
                                    .foregroundStyle(.secondary)
                            }
                            .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .buttonStyle(.bordered)
                    }
                }

                Button("Create Thread", systemImage: "plus.square") {
                    store.createThreadFromEntry(entry.id)
                }
                .font(.caption)
                .buttonStyle(.borderedProminent)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(14)
        .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 14))
        .overlay {
            RoundedRectangle(cornerRadius: 14)
                .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
        }
        .contextMenu {
            AddToListMenu(store: store, itemType: listItemType(for: entry), itemID: entry.id)
        }
    }
}

// MARK: - Workbench

private struct WorkbenchView: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord

    var body: some View {
        ScrollView {
            if let state = store.threadState(for: thread.id) {
                ThreadWorkingSurface(store: store, thread: thread, state: state)
                    .workspaceCanvasColumn()
            } else {
                SectionCard(title: "Thread", systemImage: "scope") {
                    Text("No thread state is available yet.")
                        .foregroundStyle(.secondary)
                }
                .workspaceCanvasColumn()
            }
        }
        .sheet(isPresented: $store.showingTimeline) {
            TimelineSheet(entries: store.visibleEntries(for: thread.id))
                .frame(minWidth: 520, minHeight: 520)
        }
    }
}

private struct ThreadMemoryRail: View {
    let thread: ThreadRecord
    let state: ThreadState

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            SectionCard(title: "Settled So Far", systemImage: "checklist") {
                if state.resolvedSoFar.isEmpty {
                    Text("No stable decisions have been saved yet.")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                } else {
                    VStack(alignment: .leading, spacing: 10) {
                        ForEach(state.resolvedSoFar) { item in
                            TimestampedMemoryItem(text: item.text, badge: ledgerLabel(for: item), date: item.resolvedAt)
                        }
                    }
                }
            }
        }
    }

    private func ledgerLabel(for item: ResolvedItem) -> String {
        switch item.statusLabel {
        case "Decided":
            return "Decision Made"
        case "Confirmed":
            return "Verified"
        case "Ruled out":
            return "Dropped"
        default:
            return item.statusLabel
        }
    }
}

private struct ThreadWorkingSurface: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord
    let state: ThreadState

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            HStack {
                Spacer()
                ThreadToolsMenu(store: store, thread: thread)
            }

            RestartNoteCard(restartNote: state.restartNote, recoveryLines: state.recoveryLines, lastAnchorAt: state.lastAnchorAt)

            if let preparedView = store.preparedView, preparedView.threadID == thread.id {
                PreparedViewCard(preparedView: preparedView)
            }

            WhereYouLeftOffCard(store: store, state: state)

            ThreadNotesCard(store: store, state: state)

            ContinueComposerCard(draft: $store.inlineNoteDraft) {
                store.appendInlineNote(to: thread.id)
            }
        }
    }
}

private struct ThreadToolsMenu: View {
    @Bindable var store: ThreadnoteStore
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
            Label("Thread Tools", systemImage: "ellipsis.circle")
        }
        .menuStyle(.borderlessButton)
    }
}

private struct RestartNoteCard: View {
    let restartNote: String
    let recoveryLines: [ResumeRecoveryLine]
    let lastAnchorAt: Date?

    private var bulletItems: [String] {
        if !recoveryLines.isEmpty {
            return recoveryLines.map { "\($0.title): \($0.body)" }
        }
        return restartNote
            .split(whereSeparator: \.isNewline)
            .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
            .filter { !$0.isEmpty }
    }

    var body: some View {
        SectionCard(title: "Restart Note", systemImage: "arrow.clockwise.circle") {
            VStack(alignment: .leading, spacing: 10) {
                ForEach(Array(bulletItems.enumerated()), id: \.offset) { _, item in
                    HStack(alignment: .top, spacing: 8) {
                        Circle()
                            .fill(Color.accentColor.opacity(0.65))
                            .frame(width: 6, height: 6)
                            .padding(.top, 7)
                        Text(item)
                            .font(.body.weight(.medium))
                    }
                }
                if let lastAnchorAt {
                    Text("Last useful state saved on \(lastAnchorAt.formatted(date: .abbreviated, time: .shortened)).")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
        }
    }
}

private struct WhereYouLeftOffCard: View {
    @Bindable var store: ThreadnoteStore
    let state: ThreadState

    private var lastDiscussionPoint: EntryStreamItem? {
        state.streamSections.first?.items.last
    }

    var body: some View {
        SectionCard(title: "Pick Up Here", systemImage: "arrow.turn.down.right") {
            if let item = lastDiscussionPoint {
                VStack(alignment: .leading, spacing: 12) {
                    Text("Last discussed on \(item.entry.createdAt.formatted(date: .abbreviated, time: .shortened)).")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)

                    Text("Start from the last note that moved this thread forward.")
                        .font(.caption)
                        .foregroundStyle(.secondary)

                    EntryCard(store: store, item: item)
                }
            } else {
                Text("No notes are visible yet. Use the composer below to create the first step in this thread.")
                    .foregroundStyle(.secondary)
            }
        }
    }
}

private struct ThreadNotesCard: View {
    @Bindable var store: ThreadnoteStore
    let state: ThreadState

    private var noteItems: [EntryStreamItem] {
        state.streamSections.flatMap(\.items)
    }

    var body: some View {
        SectionCard(title: "Notes", systemImage: "note.text") {
            if noteItems.isEmpty {
                Text("Notes will appear here after the first capture lands in this thread.")
                    .foregroundStyle(.secondary)
            } else {
                VStack(alignment: .leading, spacing: 12) {
                    Text("Recent notes, replies, and references in this thread.")
                        .font(.caption)
                        .foregroundStyle(.secondary)

                    ForEach(noteItems) { item in
                        EntryCard(store: store, item: item)
                    }
                }
            }
        }
    }
}

private struct ThreadMetaBadge: View {
    let label: String
    let tint: Color

    var body: some View {
        Text(label)
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(tint.opacity(0.12), in: .capsule)
            .foregroundStyle(.secondary)
    }
}

private struct TimestampedMemoryItem: View {
    let text: String
    let badge: String?
    let date: Date

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(text)
                .font(.callout)
            HStack(spacing: 8) {
                if let badge {
                    ThreadMetaBadge(label: badge, tint: .accentColor)
                }
                Text(date, format: .dateTime.year().month().day())
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }
}
private struct EntryCard: View {
    @Bindable var store: ThreadnoteStore
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
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 6) {
                    HStack(spacing: 8) {
                        EntryKindBadge(kind: item.entry.kind)
                        if let relation = item.primaryRelation {
                            RelationBadge(kind: relation.kind)
                        }
                    }

                    EntryBodyView(entry: item.entry)
                }

                Spacer()

                Text(item.entry.createdAt, style: .relative)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }

            if let citation = item.entry.sourceMetadata?.citation {
                Text(citation)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            if !item.relatedEntries.isEmpty {
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 8) {
                        ForEach(item.relatedEntries.prefix(2)) { related in
                            HStack(spacing: 6) {
                                Image(systemName: "arrowshape.turn.up.right")
                                Text(related.summaryText)
                                    .lineLimit(1)
                            }
                            .font(.caption)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 6)
                            .background(Color.accentColor.opacity(0.12), in: .capsule)
                        }
                    }
                }
            }

            if !referenceItems.isEmpty {
                VStack(alignment: .leading, spacing: 6) {
                    Text("References")
                        .font(.caption.weight(.semibold))
                        .foregroundStyle(.secondary)

                    ScrollView(.horizontal, showsIndicators: false) {
                        HStack(spacing: 8) {
                            ForEach(referenceItems) { reference in
                                ReferenceChip(reference: reference)
                            }
                        }
                    }
                }
            }

            HStack {
                if item.entry.isSourceResource {
                    Button("View Source", systemImage: "bookmark") {
                        store.openSource(item.entry.id)
                    }
                    .buttonStyle(.borderedProminent)
                }

                Button(item.replies.isEmpty ? "Reply" : "\(item.replies.count) replies") {
                    store.toggleReplies(for: item.entry.id)
                }
                .buttonStyle(.bordered)

                Spacer()
            }

            if store.expandedReplyEntryIDs.contains(item.entry.id) {
                VStack(alignment: .leading, spacing: 10) {
                    if !item.replies.isEmpty {
                        VStack(alignment: .leading, spacing: 8) {
                            ForEach(item.replies) { reply in
                                ReplyRow(entry: reply)
                            }
                        }
                    }

                    TextEditor(text: Binding(
                        get: { store.replyDrafts[item.entry.id, default: ""] },
                        set: { store.replyDrafts[item.entry.id] = $0 }
                    ))
                    .font(.body)
                    .scrollContentBackground(.hidden)
                    .frame(minHeight: 72, maxHeight: 120)
                    .padding(8)
                    .background(.thinMaterial, in: .rect(cornerRadius: 12))

                    HStack {
                        Text("Reply as a new note attached to this card.")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                        Spacer()
                        Button("Send Reply") {
                            store.appendReply(to: item.entry.id)
                        }
                        .buttonStyle(.borderedProminent)
                        .disabled(store.replyDrafts[item.entry.id, default: ""].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
                    }
                }
                .padding(.leading, 16)
            }
        }
        .padding(14)
        .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 14))
        .overlay {
            RoundedRectangle(cornerRadius: 14)
                .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
        }
        .contextMenu {
            AddToListMenu(store: store, itemType: listItemType(for: item.entry), itemID: item.entry.id)
        }
    }
}

private struct ReferenceDisplayItem: Identifiable {
    let id: String
    let label: String
    let systemImage: String
    let tint: Color
}

private struct ReferenceChip: View {
    let reference: ReferenceDisplayItem

    var body: some View {
        HStack(spacing: 6) {
            Image(systemName: reference.systemImage)
            Text(reference.label)
                .lineLimit(1)
        }
        .font(.caption)
        .padding(.horizontal, 10)
        .padding(.vertical, 6)
        .background(reference.tint.opacity(0.12), in: .capsule)
    }
}

private struct ReplyRow: View {
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack {
                EntryKindBadge(kind: entry.kind)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.caption2)
                    .foregroundStyle(.tertiary)
            }
            EntryBodyView(entry: entry)
        }
        .padding(12)
        .background(Color.secondary.opacity(0.08), in: .rect(cornerRadius: 12))
    }
}
private struct TimelineSheet: View {
    let entries: [Entry]
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 12) {
                HStack {
                    Text("Full Timeline")
                        .font(.title2.bold())
                    Spacer()
                    Button("Done") {
                        dismiss()
                    }
                    .buttonStyle(.bordered)
                }

                ForEach(entries) { entry in
                    VStack(alignment: .leading, spacing: 6) {
                        HStack {
                            EntryKindBadge(kind: entry.kind)
                            Spacer()
                            Text(entry.createdAt, style: .relative)
                                .font(.caption)
                                .foregroundStyle(.tertiary)
                        }
                        EntryBodyView(entry: entry)
                    }
                    .padding(14)
                    .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 14))
                    .overlay {
                        RoundedRectangle(cornerRadius: 14)
                            .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
                    }
                }
            }
            .padding(20)
        }
        .background(Color(nsColor: .windowBackgroundColor))
    }
}

// MARK: - Shared Components

private struct ContinueComposerCard: View {
    @Binding var draft: String
    let onSubmit: () -> Void

    var body: some View {
        SectionCard(title: "Continue", systemImage: "text.cursor") {
            CaptureComposer(
                text: $draft,
                helperText: "Use `#role`, `@object`, and `[[reference]]` to shape the note without leaving the thread.",
                submitLabel: "Add Note",
                minHeight: 132,
                submitAction: onSubmit
            )
        }
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
                    Text("• \(entry.summaryText)")
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

private struct ResumeStripRow: View {
    let label: String
    let entries: [String]

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(label)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            ForEach(entries, id: \.self) { entry in
                Text("• \(entry)")
                    .font(.callout)
            }
        }
    }
}

private struct ResumeMetricChip: View {
    let title: String
    let value: Int
    let tint: Color

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(title)
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(value, format: .number)
                .font(.headline)
                .foregroundStyle(tint)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .background(tint.opacity(0.12), in: .rect(cornerRadius: 12))
    }
}

private struct ThreadSidebarRow: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord

    var body: some View {
        Button {
            store.openThread(thread.id)
        } label: {
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text(thread.title)
                        .font(.subheadline.weight(.medium))
                        .lineLimit(2)
                }
                Spacer()
                let count = store.newEntryCount(for: thread.id)
                if count > 0 {
                    Text("\(count)")
                        .font(.caption2.weight(.bold))
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(Color.accentColor.opacity(0.2), in: .capsule)
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 10)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.thinMaterial, in: .rect(cornerRadius: 12))
            .overlay {
                RoundedRectangle(cornerRadius: 12)
                    .stroke(store.selectedThreadID == thread.id ? Color.accentColor.opacity(0.35) : Color.clear, lineWidth: 1)
            }
        }
        .buttonStyle(.plain)
        .contextMenu {
            AddToListMenu(store: store, itemType: .thread, itemID: thread.id)
        }
    }
}

private struct ListSidebarRow: View {
    @Bindable var store: ThreadnoteStore
    let list: ListRecord

    private var listSidebarSubtitle: String {
        let count = store.items(for: list.id).count
        switch list.kind {
        case .queue:
            return count == 1 ? "1 note to revisit" : "\(count) notes to revisit"
        default:
            return count == 1 ? "1 saved item" : "\(count) saved items"
        }
    }

    var body: some View {
        Button {
            store.selectList(list.id)
        } label: {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 4) {
                    Label(list.title, systemImage: listKindSymbol(for: list.kind))
                        .font(.subheadline.weight(.medium))
                        .lineLimit(2)
                    Text(listSidebarSubtitle)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                Text("\(store.items(for: list.id).count)")
                    .font(.caption2.weight(.bold))
                    .padding(.horizontal, 6)
                    .padding(.vertical, 2)
                    .background(Color.secondary.opacity(0.16), in: .capsule)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 10)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.thinMaterial, in: .rect(cornerRadius: 12))
            .overlay {
                RoundedRectangle(cornerRadius: 12)
                    .stroke(store.selectedListID == list.id ? Color.accentColor.opacity(0.35) : Color.clear, lineWidth: 1)
            }
        }
        .buttonStyle(.plain)
    }
}

private struct ListItemCard: View {
    @Bindable var store: ThreadnoteStore
    let item: ListResolvedItem

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 8) {
                    HStack(spacing: 8) {
                        ListItemTypeBadge(type: item.listItem.itemType)
                        if let entry = item.entry, shouldShowSemanticBadge(for: entry) {
                            EntryKindBadge(kind: entry.kind)
                        }
                    }

                    content
                }

                Spacer()

                VStack(alignment: .trailing, spacing: 8) {
                    Button {
                        store.togglePinned(item.listItem.id)
                    } label: {
                        Image(systemName: item.listItem.isPinned ? "pin.fill" : "pin")
                            .foregroundStyle(item.listItem.isPinned ? Color.accentColor : Color.secondary)
                    }
                    .buttonStyle(.plain)
                    .help(item.listItem.isPinned ? "Unpin from the top of this list" : "Pin to the top of this list")

                    Text(item.listItem.addedAt, format: .dateTime.year().month().day())
                        .font(.caption)
                        .foregroundStyle(.tertiary)
                }
            }

            if let note = item.listItem.note, !note.isEmpty {
                Text(note)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            footer
        }
        .padding(14)
        .background(cardBackground, in: .rect(cornerRadius: 16))
        .overlay {
            RoundedRectangle(cornerRadius: 16)
                .stroke(cardStroke, lineWidth: item.listItem.isPinned ? 1.5 : 1)
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

    @ViewBuilder
    private var content: some View {
        if let thread = item.thread {
            VStack(alignment: .leading, spacing: 6) {
                Text(thread.title)
                    .font(.headline)
                if let subtitle = threadSecondarySummary(thread) {
                    Text(subtitle)
                        .font(.body)
                        .foregroundStyle(.secondary)
                }
                Text("Updated \(thread.lastActiveAt, style: .relative)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        } else if let entry = item.entry {
            if entry.isSourceResource {
                ListSourceCardContent(store: store, entry: entry)
            } else {
                ListEntryCardContent(store: store, entry: entry)
            }
        }
    }

    @ViewBuilder
    private var footer: some View {
        if let thread = item.thread {
            Button("Open Thread", systemImage: "arrow.turn.down.right") {
                store.openThread(thread.id)
            }
            .buttonStyle(.bordered)
        } else if let entry = item.entry {
            HStack(spacing: 10) {
                if entry.isSourceResource {
                    Button("View Source", systemImage: "bookmark") {
                        store.openSource(entry.id)
                    }
                    .buttonStyle(.borderedProminent)
                }

                if let thread = store.thread(for: entry) {
                    Button {
                        store.openThread(thread.id)
                    } label: {
                        Label("In \(thread.title)", systemImage: "arrow.turn.down.right")
                            .font(.caption)
                            .frame(maxWidth: .infinity, alignment: .leading)
                    }
                    .buttonStyle(.bordered)
                }
            }

            if store.thread(for: entry) == nil {
                HStack(spacing: 6) {
                    Image(systemName: "tray")
                    Text("Unrouted")
                }
                .font(.caption.weight(.semibold))
                .foregroundStyle(.orange)
            }
        } else if item.entry != nil {
            HStack(spacing: 6) {
                Image(systemName: "tray")
                Text("Unrouted")
            }
            .font(.caption.weight(.semibold))
            .foregroundStyle(.orange)
        }
    }

    private var cardBackground: some ShapeStyle {
        if item.listItem.isPinned {
            return AnyShapeStyle(Color.accentColor.opacity(0.08))
        }
        switch item.listItem.itemType {
        case .thread:
            return AnyShapeStyle(.thinMaterial)
        case .entry:
            return AnyShapeStyle(.thinMaterial)
        case .source:
            return AnyShapeStyle(Color.purple.opacity(0.08))
        }
    }

    private var cardStroke: Color {
        if item.listItem.isPinned {
            return .accentColor.opacity(0.32)
        }
        switch item.listItem.itemType {
        case .thread:
            return .accentColor.opacity(0.12)
        case .entry:
            return .orange.opacity(0.14)
        case .source:
            return .purple.opacity(0.22)
        }
    }

    private func shouldShowSemanticBadge(for entry: Entry) -> Bool {
        !(item.listItem.itemType == .source && entry.kind == .source)
    }
}

private struct ListEntryCardContent: View {
    @Bindable var store: ThreadnoteStore
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .top, spacing: 10) {
                Image(systemName: "note.text")
                    .font(.headline)
                    .foregroundStyle(.orange)
                    .frame(width: 28, height: 28)
                    .background(Color.orange.opacity(0.12), in: .rect(cornerRadius: 10))

                VStack(alignment: .leading, spacing: 8) {
                    EntryBodyView(entry: entry)
                    if let thread = store.thread(for: entry) {
                        Label(thread.title, systemImage: "arrow.turn.down.right")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                }
            }
        }
    }
}

private struct ListSourceCardContent: View {
    @Bindable var store: ThreadnoteStore
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .top, spacing: 10) {
                Image(systemName: "bookmark.fill")
                    .font(.headline)
                    .foregroundStyle(.purple)
                    .frame(width: 28, height: 28)
                    .background(Color.purple.opacity(0.14), in: .rect(cornerRadius: 10))

                VStack(alignment: .leading, spacing: 6) {
                    Text(entry.sourceDisplayTitle)
                        .font(.headline)
                        .lineLimit(2)

                    if let locator = entry.sourceLocator {
                        Text(locator)
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .textSelection(.enabled)
                            .lineLimit(2)
                    }
                }
            }

            if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                Text(citation)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(10)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(Color.purple.opacity(0.08), in: .rect(cornerRadius: 12))
            }

            if let thread = store.thread(for: entry) {
                Label("Used in \(thread.title)", systemImage: "square.stack.3d.up")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
        }
    }
}

private struct ListFocusThreadCard: View {
    @Bindable var store: ThreadnoteStore
    let item: ListResolvedItem

    private var thread: ThreadRecord? { item.thread }

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 6) {
                    ListItemTypeBadge(type: .thread)
                    if let thread {
                        Text(thread.title)
                            .font(.headline)
                            .lineLimit(2)
                        if let subtitle = threadSecondarySummary(thread) {
                            Text(subtitle)
                                .font(.subheadline)
                                .foregroundStyle(.secondary)
                                .lineLimit(3)
                        }
                    }
                }
                Spacer()
                VStack(alignment: .trailing, spacing: 8) {
                    Button {
                        store.togglePinned(item.listItem.id)
                    } label: {
                        Image(systemName: item.listItem.isPinned ? "pin.fill" : "pin")
                            .foregroundStyle(item.listItem.isPinned ? Color.accentColor : Color.secondary)
                    }
                    .buttonStyle(.plain)
                    .help(item.listItem.isPinned ? "Unpin from the top of this list" : "Pin to the top of this list")

                    if let thread {
                        Text(thread.lastActiveAt, style: .relative)
                            .font(.caption)
                            .foregroundStyle(.tertiary)
                    }
                }
            }

            if let note = item.listItem.note, !note.isEmpty {
                Text(note)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            if let thread {
                HStack(spacing: 10) {
                    Spacer()
                    Button("Open Thread", systemImage: "arrow.turn.down.right") {
                        store.openThread(thread.id)
                    }
                    .buttonStyle(.borderedProminent)
                }
            }
        }
        .padding(16)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial, in: .rect(cornerRadius: 18))
        .overlay {
            RoundedRectangle(cornerRadius: 18)
                .stroke(item.listItem.isPinned ? Color.accentColor.opacity(0.32) : Color.accentColor.opacity(0.1), lineWidth: item.listItem.isPinned ? 1.5 : 1)
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

private struct NewListSheet: View {
    @Bindable var store: ThreadnoteStore
    @Binding var isPresented: Bool

    @State private var title = ""
    @State private var description = ""
    @State private var kind: ListKind = .pack

    var body: some View {
        VStack(alignment: .leading, spacing: 18) {
            Text("New List")
                .font(.title2.bold())

            Text("Create a lightweight resource area for threads, notes, and sources.")
                .font(.subheadline)
                .foregroundStyle(.secondary)

            VStack(alignment: .leading, spacing: 12) {
                Text("Title")
                    .font(.headline)
                TextField("Reading Queue", text: $title)
                    .textFieldStyle(.roundedBorder)

                Text("Description")
                    .font(.headline)
                TextField("What this list is organizing", text: $description)
                    .textFieldStyle(.roundedBorder)

                Text("Kind")
                    .font(.headline)
                Picker("Kind", selection: $kind) {
                    ForEach(ListKind.allCases) { option in
                        Text(option.title).tag(option)
                    }
                }
                .pickerStyle(.segmented)
            }

            Spacer()

            HStack {
                Spacer()
                Button("Cancel") {
                    isPresented = false
                }
                .buttonStyle(.bordered)

                Button("Create List") {
                    let createdID = store.createList(
                        title: title.trimmingCharacters(in: .whitespacesAndNewlines),
                        description: description.trimmingCharacters(in: .whitespacesAndNewlines),
                        kind: kind
                    )
                    store.selectList(createdID)
                    isPresented = false
                }
                .buttonStyle(.borderedProminent)
                .disabled(title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .padding(24)
        .background(Color(nsColor: .windowBackgroundColor))
    }
}

private struct ResourceOverviewRow: View {
    let items: [ListResolvedItem]

    var body: some View {
        HStack(spacing: 10) {
            Label("\(items.filter { $0.listItem.itemType == .thread }.count) threads", systemImage: "square.stack.3d.up")
            Divider()
            Label("\(items.filter { $0.listItem.itemType == .entry }.count) notes", systemImage: "note.text")
            Divider()
            Label("\(items.filter { $0.listItem.itemType == .source }.count) sources", systemImage: "bookmark")
            Spacer(minLength: 0)
        }
        .font(.caption)
        .foregroundStyle(.secondary)
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(Color.secondary.opacity(0.08), in: .rect(cornerRadius: 12))
    }
}

private struct SourceDetailSheet: View {
    @Bindable var store: ThreadnoteStore
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 18) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 8) {
                    Label("Source", systemImage: "bookmark")
                        .font(.headline)
                        .foregroundStyle(.secondary)
                    Text(entry.sourceDisplayTitle)
                        .font(.title2.bold())
                }

                Spacer()

                Button("Done") {
                    store.closeSource()
                }
                .buttonStyle(.bordered)
            }

            if let locator = entry.sourceLocator {
                SectionCard(title: "Locator", systemImage: "link") {
                    Text(locator)
                        .textSelection(.enabled)

                    if let url = URL(string: locator), let scheme = url.scheme {
                        Button(scheme.hasPrefix("http") ? "Open in Browser" : "Open Resource", systemImage: "arrow.up.right.square") {
                            NSWorkspace.shared.open(url)
                        }
                        .buttonStyle(.borderedProminent)
                    }
                }
            }

            SectionCard(title: "Summary", systemImage: "text.justify") {
                EntryBodyView(entry: entry)
                if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                    Divider()
                    Text(citation)
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
            }

            if let thread = store.thread(for: entry) {
                SectionCard(title: "Problem Space", systemImage: "square.stack.3d.up") {
                    VStack(alignment: .leading, spacing: 8) {
                        Text(thread.title)
                            .font(.headline)
                        if let subtitle = threadSecondarySummary(thread) {
                            Text(subtitle)
                                .font(.subheadline)
                                .foregroundStyle(.secondary)
                        }
                        Button("Open Thread", systemImage: "arrow.turn.down.right") {
                            store.openThread(thread.id)
                            store.closeSource()
                        }
                        .buttonStyle(.bordered)
                    }
                }
            }

            HStack {
                Spacer()
                Menu {
                    AddToListMenu(store: store, itemType: .source, itemID: entry.id)
                } label: {
                    Label("Add to List", systemImage: "plus.rectangle.on.folder")
                }
            }

            Spacer(minLength: 0)
        }
        .padding(24)
        .background(Color(nsColor: .windowBackgroundColor))
    }
}

private struct ThreadResourcesSheet: View {
    @Bindable var store: ThreadnoteStore
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
            VStack(alignment: .leading, spacing: 18) {
                HStack(alignment: .top) {
                    VStack(alignment: .leading, spacing: 8) {
                        Label("Thread Resources", systemImage: "books.vertical")
                            .font(.headline)
                            .foregroundStyle(.secondary)
                        Text(thread.title)
                            .font(.title2.bold())
                        Text("Browse the support material without turning the main thread page into a resource browser.")
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
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
                    ListSectionCard(title: "Claims", subtitle: "Active judgments inside this problem space.", systemImage: "quote.bubble") {
                        ForEach(threadClaims) { claim in
                            VStack(alignment: .leading, spacing: 6) {
                                Text(claim.statement)
                                    .font(.body)
                                Text(claim.status.rawValue.capitalized)
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                            }
                            .padding(14)
                            .frame(maxWidth: .infinity, alignment: .leading)
                            .background(.thinMaterial, in: .rect(cornerRadius: 16))
                        }
                    }
                }

                if !evidenceEntries.isEmpty {
                    ListSectionCard(title: "Evidence", subtitle: "Observations and support that move the thread forward.", systemImage: "checklist") {
                        ForEach(evidenceEntries) { entry in
                            ThreadResourceEntryRow(store: store, entry: entry)
                        }
                    }
                }

                if !sourceEntries.isEmpty {
                    ListSectionCard(title: "Sources", subtitle: "References and citations attached to this thread.", systemImage: "bookmark") {
                        ForEach(sourceEntries) { entry in
                            ThreadResourceEntryRow(store: store, entry: entry)
                        }
                    }
                }
            }
            .padding(24)
        }
        .background(Color(nsColor: .windowBackgroundColor))
        .onDisappear {
            store.closeThreadResources()
        }
    }

    private var resourceItems: [ListResolvedItem] {
        let threadItem = ListResolvedItem(
            listItem: ListItem(
                id: thread.id,
                listID: thread.id,
                itemType: .thread,
                itemID: thread.id,
                addedAt: thread.updatedAt,
                note: nil,
                position: 0,
                isPinned: false
            ),
            thread: thread,
            entry: nil
        )
        let evidenceItems = evidenceEntries.map { entry in
            ListResolvedItem(
                listItem: ListItem(
                    id: entry.id,
                    listID: thread.id,
                    itemType: .entry,
                    itemID: entry.id,
                    addedAt: entry.createdAt,
                    note: nil,
                    position: 0,
                    isPinned: false
                ),
                thread: nil,
                entry: entry
            )
        }
        let sourceItems = sourceEntries.map { entry in
            ListResolvedItem(
                listItem: ListItem(
                    id: entry.id,
                    listID: thread.id,
                    itemType: .source,
                    itemID: entry.id,
                    addedAt: entry.createdAt,
                    note: nil,
                    position: 0,
                    isPinned: false
                ),
                thread: nil,
                entry: entry
            )
        }
        return [threadItem] + evidenceItems + sourceItems
    }
}

private struct ThreadResourceEntryRow: View {
    @Bindable var store: ThreadnoteStore
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                EntryKindBadge(kind: entry.kind)
                Spacer()
                Text(entry.createdAt, style: .relative)
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }

            EntryBodyView(entry: entry)

            if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                Text(citation)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            HStack(spacing: 10) {
                if entry.isSourceResource {
                    Button("View Source", systemImage: "bookmark") {
                        store.openSource(entry.id)
                    }
                    .buttonStyle(.borderedProminent)
                }

                Menu {
                    AddToListMenu(store: store, itemType: listItemType(for: entry), itemID: entry.id)
                } label: {
                    Label("Add to List", systemImage: "plus.rectangle.on.folder")
                }
            }
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 14))
        .overlay {
            RoundedRectangle(cornerRadius: 14)
                .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
        }
    }
}

private struct ListResolvedSection: Identifiable {
    let id = UUID()
    let title: String
    let subtitle: String
    let systemImage: String
    let items: [ListResolvedItem]
}

private struct ListSectionCard<Content: View>: View {
    let title: String
    let subtitle: String
    let systemImage: String
    @ViewBuilder let content: Content

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            VStack(alignment: .leading, spacing: 4) {
                Label(title, systemImage: systemImage)
                    .font(.headline)
                Text(subtitle)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            content
        }
        .padding(18)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 18))
        .overlay {
            RoundedRectangle(cornerRadius: 18)
                .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
        }
    }
}

private struct AddToListMenu: View {
    @Bindable var store: ThreadnoteStore
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

private struct EntryKindBadge: View {
    let kind: EntryKind

    var body: some View {
        Text(kind.title)
            .font(.caption.weight(.semibold))
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(badgeBackground, in: .capsule)
            .foregroundStyle(badgeForeground)
    }

    private var badgeBackground: Color {
        switch kind {
        case .note:
            Color.secondary.opacity(0.12)
        case .idea:
            Color.yellow.opacity(0.18)
        case .question:
            Color.blue.opacity(0.18)
        case .claim:
            Color.orange.opacity(0.18)
        case .evidence:
            Color.green.opacity(0.18)
        case .source:
            Color.purple.opacity(0.18)
        case .comparison:
            Color.cyan.opacity(0.18)
        case .pattern:
            Color.indigo.opacity(0.18)
        case .plan:
            Color.teal.opacity(0.18)
        case .decided:
            Color.accentColor.opacity(0.18)
        case .solved:
            Color.green.opacity(0.22)
        case .verified:
            Color.mint.opacity(0.22)
        case .dropped:
            Color.red.opacity(0.18)
        case .handoff, .anchorWritten:
            Color.secondary.opacity(0.12)
        }
    }

    private var badgeForeground: Color {
        switch kind {
        case .note:
            .secondary
        case .idea:
            .yellow
        case .question:
            .blue
        case .claim:
            .orange
        case .evidence:
            .green
        case .source:
            .purple
        case .comparison:
            .cyan
        case .pattern:
            .indigo
        case .plan:
            .teal
        case .decided:
            .accentColor
        case .solved:
            .green
        case .verified:
            .mint
        case .dropped:
            .red
        case .handoff, .anchorWritten:
            .secondary
        }
    }
}

private struct ListItemTypeBadge: View {
    let type: ListItemType

    var body: some View {
        Text(title)
            .font(.caption.weight(.semibold))
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(color.opacity(0.14), in: .capsule)
            .foregroundStyle(color)
    }

    private var title: String {
        switch type {
        case .thread:
            "Thread"
        case .entry:
            "Note"
        case .source:
            "Source"
        }
    }

    private var color: Color {
        switch type {
        case .thread:
            .accentColor
        case .entry:
            .orange
        case .source:
            .purple
        }
    }
}

private struct RelationBadge: View {
    let kind: DiscourseRelationKind

    var body: some View {
        Label(kind.title, systemImage: symbol)
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(Color.accentColor.opacity(0.12), in: .capsule)
            .foregroundStyle(.secondary)
    }

    private var symbol: String {
        switch kind {
        case .supports:
            "arrow.up.right.circle"
        case .opposes:
            "minus.circle"
        case .informs:
            "info.circle"
        case .answers:
            "checkmark.bubble"
        }
    }
}

private struct EntryBodyView: View {
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            switch entry.body.kind {
            case .text:
                Text(entry.summaryText)
                    .font(.body)
            case .url:
                if let urlString = entry.body.url, let url = URL(string: urlString) {
                    Link(destination: url) {
                        Label(entry.sourceMetadata?.title ?? url.host() ?? urlString, systemImage: "link")
                    }
                    .font(.body)
                    if let locator = entry.sourceMetadata?.locator {
                        Text(locator)
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                } else {
                    Text(entry.summaryText)
                        .font(.body)
                }
            case .image:
                Label(entry.body.title ?? "Image", systemImage: "photo")
                    .font(.body)
                if let details = entry.body.details {
                    Text(details)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            case .document:
                Label(entry.body.title ?? "Document", systemImage: "doc.richtext")
                    .font(.body)
                if let details = entry.body.details {
                    Text(details)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            case .mixed:
                if let text = entry.body.text {
                    Text(text)
                        .font(.body)
                }
                if let urlString = entry.body.url, let url = URL(string: urlString) {
                    Link(destination: url) {
                        Label(entry.sourceMetadata?.title ?? url.host() ?? urlString, systemImage: "link")
                    }
                    .font(.caption)
                }
            }
        }
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
        .background(Color(nsColor: .windowBackgroundColor), in: .rect(cornerRadius: 18))
        .overlay {
            RoundedRectangle(cornerRadius: 18)
                .stroke(Color.secondary.opacity(0.12), lineWidth: 1)
        }
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

private func threadSecondarySummary(_ thread: ThreadRecord) -> String? {
    let title = thread.title.trimmingCharacters(in: .whitespacesAndNewlines)
    let goal = thread.goalLayer.goalStatement.trimmingCharacters(in: .whitespacesAndNewlines)
    guard !goal.isEmpty else { return nil }
    guard goal.compare(title, options: [.caseInsensitive, .diacriticInsensitive]) != .orderedSame else { return nil }
    return goal
}

private func threadSubtitle(for thread: ThreadRecord) -> String {
    threadSecondarySummary(thread) ?? thread.goalLayer.goalType.title
}

private func listItemType(for entry: Entry) -> ListItemType {
    entry.kind == .source ? .source : .entry
}

private func listKindSymbol(for kind: ListKind) -> String {
    switch kind {
    case .topic:
        "number"
    case .queue:
        "line.3.horizontal.decrease.circle"
    case .pack:
        "shippingbox"
    case .collection:
        "square.stack"
    }
}
