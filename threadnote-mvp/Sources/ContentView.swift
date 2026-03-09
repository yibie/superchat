import AppKit
import SwiftUI

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
                if store.isInWorkbench, let thread = store.selectedThread {
                    WorkbenchView(store: store, thread: thread)
                } else {
                    StreamView(store: store)
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
        .onChange(of: scenePhase) { _, newPhase in
            if newPhase != .active {
                store.maybeWriteAnchorIfNeeded(for: store.selectedThreadID)
            }
        }
    }

    private var header: some View {
        HStack(spacing: 16) {
            if store.isInWorkbench {
                Button("Stream", systemImage: "chevron.left") {
                    store.goToStream()
                }
                .buttonStyle(.borderless)
            }

            VStack(alignment: .leading, spacing: 2) {
                Text(store.isInWorkbench ? (store.selectedThread?.title ?? "Thread") : "Threadnote")
                    .font(.title2.bold())
                Text(headerSubtitle)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
            }

            Spacer()

            if store.isInWorkbench, let threadID = store.selectedThreadID {
                Menu {
                    ForEach(PreparedViewType.allCases) { type in
                        Button(type.title) {
                            store.prepareView(type: type, for: threadID)
                        }
                    }
                } label: {
                    Label("Prepare", systemImage: "doc.text.magnifyingglass")
                }

                if store.preparedView != nil {
                    Button("Exit Prepare") {
                        store.preparedView = nil
                    }
                    .buttonStyle(.bordered)
                }
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
        if store.isInWorkbench {
            return store.selectedThread?.prompt ?? "Resume one problem at a time."
        }
        return "Capture first. The system routes it."
    }

    private func openQuickCapture() {
        store.beginCapture()
        openWindow(id: "capture")
    }
}

// MARK: - Stream

private struct StreamView: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        HSplitView {
            ThreadListSidebar(store: store)
                .frame(minWidth: 220, idealWidth: 260, maxWidth: 320)
            StreamContentView(store: store)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }
}

private struct ThreadListSidebar: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        SidebarSurface {
            Text("Threads")
                .font(.headline)
                .padding(.bottom, 8)

            ScrollView {
                VStack(alignment: .leading, spacing: 6) {
                    ForEach(store.homeThreads) { thread in
                        Button {
                            store.openThread(thread.id)
                        } label: {
                            HStack {
                                VStack(alignment: .leading, spacing: 4) {
                                    Text(thread.title)
                                        .font(.subheadline.weight(.medium))
                                        .lineLimit(2)
                                    Text(thread.status.rawValue.capitalized)
                                        .font(.caption)
                                        .foregroundStyle(.secondary)
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
                        }
                        .buttonStyle(.plain)
                    }

                    if !store.inboxEntries.isEmpty {
                        Divider()
                            .padding(.vertical, 4)
                        Label("\(store.inboxEntries.count) unrouted", systemImage: "tray")
                            .font(.caption)
                            .foregroundStyle(.orange)
                    }
                }
            }
        }
    }
}

private struct StreamContentView: View {
    @Bindable var store: ThreadnoteStore

    var body: some View {
        VStack(spacing: 0) {
            StreamInputBox(store: store)

            Divider()
                .padding(.vertical, 8)

            ScrollView {
                LazyVStack(alignment: .leading, spacing: 10) {
                    ForEach(store.streamEntries) { entry in
                        StreamEntryRow(store: store, entry: entry)
                    }
                }
            }
        }
    }
}

private struct StreamInputBox: View {
    @Bindable var store: ThreadnoteStore
    @State private var draft = ""
    @FocusState private var isFocused: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            TextEditor(text: $draft)
                .font(.body)
                .scrollContentBackground(.hidden)
                .frame(minHeight: 60, maxHeight: 120)
                .padding(10)
                .background(.thinMaterial, in: .rect(cornerRadius: 14))
                .focused($isFocused)

            HStack {
                Text("Write the thought. The system routes it automatically.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
                Button("Send") {
                    submitStreamCapture()
                }
                .buttonStyle(.borderedProminent)
                .keyboardShortcut(.return, modifiers: .command)
                .disabled(draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
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
        VStack(alignment: .leading, spacing: 8) {
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

            if let thread = store.thread(for: entry) {
                Button {
                    store.openThread(thread.id)
                } label: {
                    Label(thread.title, systemImage: "arrow.turn.down.right")
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
                            Label(suggestion.thread.title, systemImage: "sparkles")
                                .font(.caption)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .buttonStyle(.bordered)
                    }
                }

                Button("Create Thread", systemImage: "square.stack.badge.plus") {
                    store.createThreadFromEntry(entry.id)
                }
                .font(.caption)
                .buttonStyle(.borderedProminent)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(14)
        .background(.thinMaterial, in: .rect(cornerRadius: 16))
    }
}

// MARK: - Workbench

private struct WorkbenchView: View {
    @Bindable var store: ThreadnoteStore
    let thread: ThreadRecord

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                if let preparedView = store.preparedView, preparedView.threadID == thread.id {
                    PreparedViewCard(preparedView: preparedView)
                } else if let state = store.threadState(for: thread.id) {
                    AnchorCard(state: state)
                }

                let delta = store.deltaEntries(for: thread.id)
                if !delta.isEmpty {
                    RecentDeltaCard(entries: delta)
                }

                ContinueComposerCard(draft: $store.inlineNoteDraft) {
                    store.appendInlineNote(to: thread.id)
                }

                FullTimelineCard(entries: store.visibleEntries(for: thread.id))
            }
        }
    }
}

private struct AnchorCard: View {
    let state: ThreadState

    var body: some View {
        SectionCard(title: "Current State", systemImage: "scope") {
            Text(state.coreQuestion)
                .font(.headline)

            Divider()

            Text(state.currentJudgment)
                .font(.body)

            if !state.keyClaims.isEmpty {
                Divider()
                Text("Claims")
                    .font(.subheadline.weight(.semibold))
                ForEach(state.keyClaims) { claim in
                    HStack(alignment: .top, spacing: 6) {
                        Image(systemName: "checkmark.seal")
                            .foregroundStyle(.secondary)
                        Text(claim.statement)
                    }
                    .font(.callout)
                }
            }

            if !state.openLoops.isEmpty {
                Divider()
                Text("Open Loops")
                    .font(.subheadline.weight(.semibold))
                ForEach(state.openLoops, id: \.self) { loop in
                    Text("• \(loop)")
                        .font(.callout)
                }
            }

            if let next = state.nextAction {
                Divider()
                Label("Next: \(next)", systemImage: "arrow.right.circle")
                    .font(.callout.weight(.medium))
            }
        }
    }
}

private struct RecentDeltaCard: View {
    let entries: [Entry]

    var body: some View {
        SectionCard(title: "Since Last Checkpoint (\(entries.count))", systemImage: "clock.arrow.circlepath") {
            ForEach(entries) { entry in
                VStack(alignment: .leading, spacing: 4) {
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
                        .font(.callout)
                }
                .padding(.vertical, 4)
            }
        }
    }
}

private struct FullTimelineCard: View {
    let entries: [Entry]

    var body: some View {
        DisclosureGroup {
            VStack(alignment: .leading, spacing: 8) {
                ForEach(entries) { entry in
                    VStack(alignment: .leading, spacing: 4) {
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
                            .font(.callout)
                    }
                    .padding(.vertical, 2)
                    Divider()
                }
            }
        } label: {
            Label("Full Timeline (\(entries.count) entries)", systemImage: "clock")
                .font(.headline)
        }
        .padding(18)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.regularMaterial, in: .rect(cornerRadius: 20))
    }
}

// MARK: - Shared Components

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
