import SwiftUI

// MARK: - ThreadSidecarView
//
// The persistent right-column sidecar for a thread.
// Rendered as the NavigationSplitView detail column — macOS handles the
// draggable divider natively. Tabs: Timeline | Resources.

struct ThreadSidecarView: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    private var selectedTab: ThreadSidebarTabRole {
        store.threadSidebar.selectedTab.role
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            sidecarTabBar

            ThinDivider()

            ScrollView {
                sidecarContent
                    .padding(TNSpacing.md)
            }
        }
        .background(Color.tnBackground)
        .navigationTitle(thread.title)
        .navigationSubtitle(selectedTab == .timeline ? "Timeline" : "Resources")
    }

    // MARK: - Tab bar

    private var sidecarTabBar: some View {
        HStack(spacing: 0) {
            SidecarTabButton(
                label: "Timeline",
                systemImage: "clock",
                isSelected: selectedTab == .timeline
            ) {
                store.openThreadSidebar(.timeline, for: thread.id)
            }

            SidecarTabButton(
                label: "Resources",
                systemImage: "books.vertical",
                isSelected: selectedTab == .resources
            ) {
                store.openThreadSidebar(.resources, for: thread.id)
            }

            Spacer()
        }
        .padding(.horizontal, TNSpacing.sm)
        .padding(.top, TNSpacing.xs)
    }

    // MARK: - Content

    @ViewBuilder
    private var sidecarContent: some View {
        switch selectedTab {
        case .timeline:
            TimelinePanel(entries: store.visibleEntries(for: thread.id))
        case .resources:
            ThreadResourcesPanel(thread: thread)
        case .extra:
            ContentUnavailableView(
                "No content yet",
                systemImage: "square.dashed",
                description: Text("Reserved for future use.")
            )
            .padding(.vertical, TNSpacing.xxl)
        }
    }
}

// MARK: - SidecarTabButton

private struct SidecarTabButton: View {
    let label: String
    let systemImage: String
    let isSelected: Bool
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            HStack(spacing: TNSpacing.xs) {
                Image(systemName: systemImage)
                Text(label)
            }
            .font(.tnCaption.weight(.medium))
            .foregroundStyle(isSelected ? Color.primary : .secondary)
            .padding(.horizontal, TNSpacing.sm)
            .padding(.vertical, 8)
            .background(
                isSelected ? Color.tnSurface : Color.clear,
                in: .rect(cornerRadius: TNCorner.sm)
            )
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.sm)
                    .stroke(isSelected ? Color.tnBorder : Color.clear, lineWidth: 1)
            )
        }
        .buttonStyle(.plain)
    }
}
