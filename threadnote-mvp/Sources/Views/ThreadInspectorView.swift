import SwiftUI

// MARK: - ThreadSidecarView
//
// The persistent right-column sidecar for a thread.
// Rendered as the NavigationSplitView detail column — macOS handles the
// draggable divider natively. Tabs: Restart Note | Resources.

struct ThreadSidecarView: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    private var selectedTabModel: ThreadSidebarTab {
        store.threadSidebar.selectedTab
    }

    private var selectedTab: ThreadSidebarTabRole {
        selectedTabModel.role
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
        .navigationSubtitle(selectedTabModel.title)
    }

    // MARK: - Tab bar

    private var sidecarTabBar: some View {
        HStack(spacing: 0) {
            SidecarTabButton(
                label: "Restart Note",
                systemImage: "arrow.clockwise.circle",
                isSelected: selectedTab == .restartNote
            ) {
                store.openThreadSidebar(.restartNote, for: thread.id)
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
        case .restartNote:
            if let state = store.threadState(for: thread.id) {
                RestartNoteView(state: state)
            } else {
                ContentUnavailableView(
                    "No restart note yet",
                    systemImage: "arrow.clockwise.circle",
                    description: Text("Open or update this thread to generate a fresh restart note.")
                )
                .padding(.vertical, TNSpacing.xxl)
            }
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

private struct RestartNoteView: View {
    let state: ThreadState
    @State private var showingAIDebug = true

    var body: some View {
        if state.contentState.status != .ready {
            VStack(alignment: .leading, spacing: TNSpacing.sm) {
                aiContentStateView(state.contentState)
                aiDebugSection(state.aiDebug)
            }
        } else {
            let presentation = state.presentation

            VStack(alignment: .leading, spacing: TNSpacing.sm) {
                if !presentation.headline.isEmpty {
                    Text(presentation.headline)
                        .font(.tnBody.weight(.medium))
                        .fixedSize(horizontal: false, vertical: true)
                }

                if !presentation.blocks.isEmpty {
                    LazyVGrid(
                        columns: [GridItem(.adaptive(minimum: 240), spacing: TNSpacing.md)],
                        alignment: .leading,
                        spacing: TNSpacing.md
                    ) {
                        ForEach(presentation.blocks) { block in
                            threadPresentationBlock(block)
                        }
                    }
                }

                HStack(spacing: TNSpacing.xs) {
                    if let lastAnchorAt = state.lastAnchorAt {
                        Text("Last saved \(lastAnchorAt.formatted(date: .abbreviated, time: .shortened))")
                            .font(.tnMicro)
                            .foregroundStyle(.tertiary)
                    }
                    aiStatusBadge(state.aiDebug)
                }

                aiDebugSection(state.aiDebug)
            }
        }
    }

    @ViewBuilder
    private func threadPresentationBlock(_ block: ThreadBlock) -> some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            Text(block.title)
                .font(.tnMicro.weight(.semibold))
                .foregroundStyle(blockToneForeground(block.tone))

            if let summary = block.summary, !summary.isEmpty {
                Text(summary)
                    .font(block.kind == .judgment ? .tnBody.weight(.semibold) : .tnBody)
                    .foregroundStyle(.primary)
                    .fixedSize(horizontal: false, vertical: true)
            }

            if !block.items.isEmpty {
                VStack(alignment: .leading, spacing: TNSpacing.xs) {
                    ForEach(Array(block.items.enumerated()), id: \.offset) { _, item in
                        HStack(alignment: .top, spacing: TNSpacing.sm) {
                            Circle()
                                .fill(blockToneDot(block.tone))
                                .frame(width: 5, height: 5)
                                .padding(.top, 7)
                            Text(item)
                                .font(.tnBody)
                                .fixedSize(horizontal: false, vertical: true)
                        }
                    }
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(TNSpacing.md)
        .background(blockToneFill(block.tone), in: .rect(cornerRadius: 16))
        .overlay(
            RoundedRectangle(cornerRadius: 16)
                .stroke(blockToneStroke(block.tone), lineWidth: 1)
        )
    }

    @ViewBuilder
    private func aiContentStateView(_ state: AIContentState) -> some View {
        HStack(alignment: .top, spacing: TNSpacing.sm) {
            if state.status == .loading {
                ProgressView().controlSize(.mini)
                    .padding(.top, 1)
            } else {
                Image(systemName: aiContentStateIcon(state.status))
                    .font(.system(size: 12, weight: .semibold))
                    .foregroundStyle(aiContentStateColor(state.status))
                    .padding(.top, 2)
            }

            Text(state.message)
                .font(.tnBody)
                .foregroundStyle(aiContentStateColor(state.status))
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    @ViewBuilder
    private func aiStatusBadge(_ debug: ThreadAIDebugState) -> some View {
        HStack(spacing: 4) {
            if debug.status == .pending {
                ProgressView().controlSize(.mini)
            } else {
                Image(systemName: aiStatusIcon(debug.status))
                    .font(.system(size: 10, weight: .semibold))
            }
            Text(debug.message)
                .font(.tnMicro)
                .foregroundStyle(aiStatusColor(debug.status))
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    @ViewBuilder
    private func aiDebugSection(_ debug: ThreadAIDebugState) -> some View {
        DisclosureGroup(isExpanded: $showingAIDebug) {
            VStack(alignment: .leading, spacing: TNSpacing.sm) {
                aiDebugField("Status", value: debug.status.rawValue)
                aiDebugField("Backend", value: debug.backendLabel)
                aiDebugField("Configured Model", value: debug.configuredModelID)
                aiDebugField("Response Model", value: debug.responseModelID)
                aiDebugField("Response ID", value: debug.responseID)
                aiDebugField("Finish Reason", value: debug.finishReason)
                aiDebugField("Updated", value: debug.updatedAt.formatted(date: .abbreviated, time: .standard))

                if !debug.warnings.isEmpty {
                    aiDebugField("Warnings", value: debug.warnings.joined(separator: "\n"))
                }

                aiDebugTextBlock("Parsed Response", value: debug.parsedResponse)
                aiDebugTextBlock("Raw Response Body", value: debug.rawResponseBody)
            }
            .padding(.top, TNSpacing.sm)
        } label: {
            Label("AI Debug", systemImage: "ladybug")
                .font(.tnMicro.weight(.semibold))
                .foregroundStyle(.secondary)
        }
        .padding(.top, TNSpacing.sm)
    }

    @ViewBuilder
    private func aiDebugField(_ title: String, value: String?) -> some View {
        if let value, !value.isEmpty {
            VStack(alignment: .leading, spacing: 4) {
                Text(title)
                    .font(.tnMicro.weight(.semibold))
                    .foregroundStyle(.secondary)
                Text(value)
                    .font(.tnMicro)
                    .foregroundStyle(.primary)
                    .textSelection(.enabled)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
    }

    @ViewBuilder
    private func aiDebugTextBlock(_ title: String, value: String?) -> some View {
        if let value, !value.isEmpty {
            VStack(alignment: .leading, spacing: 4) {
                Text(title)
                    .font(.tnMicro.weight(.semibold))
                    .foregroundStyle(.secondary)
                ScrollView(.vertical) {
                    Text(verbatim: value)
                        .font(.system(.caption, design: .monospaced))
                        .foregroundStyle(.primary)
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .textSelection(.enabled)
                }
                .frame(maxHeight: 180)
                .padding(TNSpacing.sm)
                .background(Color.white.opacity(0.03), in: .rect(cornerRadius: 12))
                .overlay(
                    RoundedRectangle(cornerRadius: 12)
                        .stroke(Color.white.opacity(0.08), lineWidth: 1)
                )
            }
        }
    }

    private func aiContentStateIcon(_ status: AIContentStatus) -> String {
        switch status {
        case .notConfigured:
            return "wrench.and.screwdriver.fill"
        case .loading:
            return "clock.fill"
        case .ready:
            return "checkmark.circle.fill"
        case .error:
            return "xmark.octagon.fill"
        }
    }

    private func aiContentStateColor(_ status: AIContentStatus) -> Color {
        switch status {
        case .notConfigured:
            return .orange
        case .loading:
            return .secondary
        case .ready:
            return .green
        case .error:
            return .red
        }
    }

    private func aiStatusIcon(_ status: ThreadAIStatus) -> String {
        switch status {
        case .notConfigured:
            return "exclamationmark.triangle.fill"
        case .pending:
            return "clock.fill"
        case .applied:
            return "checkmark.circle.fill"
        case .invalidPlan:
            return "exclamationmark.bubble.fill"
        case .failed:
            return "xmark.octagon.fill"
        }
    }

    private func aiStatusColor(_ status: ThreadAIStatus) -> Color {
        switch status {
        case .notConfigured, .invalidPlan:
            return .orange
        case .pending:
            return .secondary
        case .applied:
            return .green
        case .failed:
            return .red
        }
    }

    private func blockToneFill(_ tone: ThreadBlockTone) -> Color {
        switch tone {
        case .neutral:
            return Color.white.opacity(0.04)
        case .accent:
            return Color.accentColor.opacity(0.12)
        case .warning:
            return Color.orange.opacity(0.10)
        case .success:
            return Color.green.opacity(0.10)
        case .subdued:
            return Color.white.opacity(0.025)
        }
    }

    private func blockToneStroke(_ tone: ThreadBlockTone) -> Color {
        switch tone {
        case .neutral:
            return Color.white.opacity(0.08)
        case .accent:
            return Color.accentColor.opacity(0.24)
        case .warning:
            return Color.orange.opacity(0.28)
        case .success:
            return Color.green.opacity(0.28)
        case .subdued:
            return Color.white.opacity(0.06)
        }
    }

    private func blockToneForeground(_ tone: ThreadBlockTone) -> Color {
        switch tone {
        case .neutral:
            return Color.white.opacity(0.75)
        case .accent:
            return Color.accentColor
        case .warning:
            return Color.orange
        case .success:
            return Color.green
        case .subdued:
            return Color.white.opacity(0.6)
        }
    }

    private func blockToneDot(_ tone: ThreadBlockTone) -> Color {
        blockToneForeground(tone).opacity(0.75)
    }
}
