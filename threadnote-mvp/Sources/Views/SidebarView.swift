import SwiftUI

struct SidebarView: View {
    @Environment(ThreadnoteStore.self) private var store
    @State private var showingSettings = false

    var body: some View {
        List {
            SidebarInboxSection()
            SidebarResourcesSection()
            SidebarThreadsSection()
        }
        .listStyle(.sidebar)
        .safeAreaInset(edge: .bottom) {
            HStack {
                Spacer()
                Button {
                    showingSettings = true
                } label: {
                    Image(systemName: "gear")
                        .font(.system(size: 14))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .padding(8)
            }
        }
        .sheet(isPresented: $showingSettings) {
            AISettingsView()
        }
    }
}

private struct SidebarInboxSection: View {
    @Environment(ThreadnoteStore.self) private var store

    private var isSelected: Bool {
        !store.isInWorkbench && store.selectedHomeSurface == .inbox
    }

    var body: some View {
        Section {
            Button {
                store.goToStream()
            } label: {
                HStack {
                    Label("Inbox", systemImage: "tray")
                    Spacer()
                    if store.inboxEntries.count > 0 {
                        Text("\(store.inboxEntries.count)")
                            .font(.tnMicro.weight(.bold))
                            .foregroundStyle(.white)
                            .padding(.horizontal, 6)
                            .padding(.vertical, 2)
                            .background(.orange, in: .capsule)
                    }
                }
            }
            .buttonStyle(.plain)
            .listRowBackground(sidebarHighlight(isSelected))
        }
    }
}

private struct SidebarResourcesSection: View {
    @Environment(ThreadnoteStore.self) private var store

    private var resourceCount: Int {
        store.allResources.count
    }

    private var isSelected: Bool {
        !store.isInWorkbench && store.selectedHomeSurface == .resources
    }

    var body: some View {
        Section {
            Button {
                store.openResources()
            } label: {
                HStack {
                    Label("Resources", systemImage: "books.vertical")
                    Spacer()
                    if resourceCount > 0 {
                        Text("\(resourceCount)")
                            .font(.tnMicro.weight(.bold))
                            .foregroundStyle(Color.accentColor)
                            .padding(.horizontal, 6)
                            .padding(.vertical, 2)
                            .background(Color.accentColor.opacity(0.12), in: .capsule)
                    }
                }
            }
            .buttonStyle(.plain)
            .listRowBackground(sidebarHighlight(isSelected))
        }
    }
}

private struct SidebarThreadsSection: View {
    @Environment(ThreadnoteStore.self) private var store

    var body: some View {
        Section {
            ForEach(store.homeThreads) { thread in
                SidebarThreadRow(thread: thread)
            }
        } header: {
            HStack {
                Text("Threads")
                Spacer()
                Button {
                    store.beginThreadCreation()
                } label: {
                    Image(systemName: "plus")
                }
                .buttonStyle(.plain)
            }
        }
    }
}

private struct SidebarThreadRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    var body: some View {
        Button {
            store.openThread(thread.id)
        } label: {
            HStack {
                Circle()
                    .fill(thread.color.color)
                    .frame(width: 10, height: 10)
                Text(thread.title)
                    .lineLimit(2)
                Spacer()
                let count = store.newEntryCount(for: thread.id)
                if count > 0 {
                    Text("\(count)")
                        .font(.tnMicro.weight(.bold))
                        .foregroundStyle(thread.color.color)
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(thread.color.color.opacity(0.12), in: .capsule)
                }
            }
        }
        .buttonStyle(.plain)
        .listRowBackground(sidebarHighlight(store.selectedThreadID == thread.id, color: thread.color.color))
    }
}

@ViewBuilder
private func sidebarHighlight(_ isSelected: Bool, color: Color = .accentColor) -> some View {
    if isSelected {
        HStack(spacing: 0) {
            Rectangle().fill(color).frame(width: 2)
            color.opacity(0.08)
        }
        .clipShape(.rect(cornerRadius: TNCorner.sm))
    } else {
        Color.clear
    }
}
