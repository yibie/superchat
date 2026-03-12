import SwiftUI

struct SidebarView: View {
    @Environment(ThreadnoteStore.self) private var store
    @State private var showingNewListSheet = false

    var body: some View {
        List {
            SidebarInboxSection()
            SidebarThreadsSection()
            SidebarListsSection(showingNewListSheet: $showingNewListSheet)
        }
        .listStyle(.sidebar)
        .sheet(isPresented: $showingNewListSheet) {
            NewListSheet(isPresented: $showingNewListSheet)
                .frame(minWidth: 420, minHeight: 320)
        }
    }
}

// MARK: - Inbox Section

private struct SidebarInboxSection: View {
    @Environment(ThreadnoteStore.self) private var store

    private var isSelected: Bool {
        !store.isInWorkbench && store.selectedList == nil
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

// MARK: - Threads Section

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
                Text(thread.title)
                    .lineLimit(2)
                Spacer()
                let count = store.newEntryCount(for: thread.id)
                if count > 0 {
                    Text("\(count)")
                        .font(.tnMicro.weight(.bold))
                        .foregroundStyle(Color.accentColor)
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(Color.accentColor.opacity(0.12), in: .capsule)
                }
            }
        }
        .buttonStyle(.plain)
        .listRowBackground(sidebarHighlight(store.selectedThreadID == thread.id))
        .contextMenu {
            AddToListMenu(itemType: .thread, itemID: thread.id)
        }
    }
}

// MARK: - Lists Section

private struct SidebarListsSection: View {
    @Environment(ThreadnoteStore.self) private var store
    @Binding var showingNewListSheet: Bool

    var body: some View {
        Section {
            if store.lists.isEmpty {
                Text("No lists yet")
                    .font(.tnCaption)
                    .foregroundStyle(.tertiary)
            } else {
                ForEach(store.sortedLists) { list in
                    SidebarListRow(list: list)
                }
            }
        } header: {
            HStack {
                Text("Lists")
                Spacer()
                Button {
                    showingNewListSheet = true
                } label: {
                    Image(systemName: "plus")
                }
                .buttonStyle(.plain)
            }
        }
    }
}

private struct SidebarListRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let list: ListRecord

    var body: some View {
        Button {
            store.selectList(list.id)
        } label: {
            Label(list.title, systemImage: listKindSymbol(for: list.kind))
        }
        .buttonStyle(.plain)
        .listRowBackground(sidebarHighlight(store.selectedListID == list.id))
    }
}

// MARK: - Shared helper

@ViewBuilder
private func sidebarHighlight(_ isSelected: Bool) -> some View {
    if isSelected {
        HStack(spacing: 0) {
            Rectangle().fill(Color.accentColor).frame(width: 2)
            Color.accentColor.opacity(0.08)
        }
        .clipShape(.rect(cornerRadius: TNCorner.sm))
    } else {
        Color.clear
    }
}
