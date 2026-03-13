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
        } content: {
            DocumentCanvas {
                currentDocument(threadState: cachedThreadState)
            }
            .navigationSplitViewColumnWidth(min: 400, ideal: 560)
        } detail: {
            if store.isInWorkbench, let thread = store.selectedThread {
                ThreadSidecarView(thread: thread)
                    .navigationSplitViewColumnWidth(min: 280, ideal: 360, max: 520)
            } else {
                ContentUnavailableView(
                    "No thread selected",
                    systemImage: "rectangle.stack",
                    description: Text("Select a thread to see its timeline and resources here.")
                )
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
    }

    private var selectedThreadState: ThreadState? {
        guard let threadID = store.selectedThreadID else { return nil }
        return store.threadState(for: threadID)
    }

    @ViewBuilder
    private func currentDocument(threadState: ThreadState?) -> some View {
        if store.isInWorkbench, let thread = store.selectedThread {
            ThreadDocument(thread: thread, state: threadState)
        } else if store.selectedHomeSurface == .resources {
            ResourcesDocument()
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
}
