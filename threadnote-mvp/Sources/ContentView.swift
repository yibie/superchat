import AppKit
import SwiftUI

struct ContentView: View {
    @Environment(ThreadnoteStore.self) private var store
    @Environment(\.scenePhase) private var scenePhase

    var body: some View {
        @Bindable var store = store
        let cachedThreadState = selectedThreadState

        NavigationSplitView {
            SidebarView()
                .navigationSplitViewColumnWidth(min: 200, ideal: 240, max: 300)
        } detail: {
            DocumentCanvas {
                currentDocument(threadState: cachedThreadState)
            }
        }
        .background(Color.tnBackground)
        .onChange(of: scenePhase) { _, newPhase in
            if newPhase != .active {
                store.maybeWriteAnchorIfNeeded(for: store.selectedThreadID)
            }
        }
        .sheet(item: $store.threadCreationContext) { context in
            NewThreadSheet(context: context)
                .frame(minWidth: 460, minHeight: 420)
        }
        .sheet(isPresented: sourceSheetBinding) {
            if let entry = store.selectedSourceEntry {
                SourceDetailSheet(entry: entry)
                    .frame(minWidth: 520, minHeight: 420)
            }
        }
        .sheet(item: resourcesThreadBinding) { thread in
            ThreadResourcesSheet(thread: thread)
                .frame(minWidth: 560, minHeight: 520)
        }
    }

    private var selectedThreadState: ThreadState? {
        guard let threadID = store.selectedThreadID else { return nil }
        return store.threadState(for: threadID)
    }

    @ViewBuilder
    private func currentDocument(threadState: ThreadState?) -> some View {
        if store.isInWorkbench, let thread = store.selectedThread {
            ThreadDocument(thread: thread, state: threadState)
        } else if let list = store.selectedList {
            ListDocument(list: list)
        } else {
            StreamDocument()
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
