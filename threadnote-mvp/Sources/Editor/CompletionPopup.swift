import AppKit
import SwiftUI

struct CompletionPopupContent: View {
    let items: [CompletionItem]
    let highlightedIndex: Int
    let onChoose: (CompletionItem) -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            ForEach(Array(items.prefix(6).enumerated()), id: \.element.id) { index, item in
                Button {
                    onChoose(item)
                } label: {
                    HStack(spacing: 8) {
                        Image(systemName: item.icon)
                            .foregroundStyle(iconColor(for: item.kind))
                            .frame(width: 16)

                        VStack(alignment: .leading, spacing: 1) {
                            Text(item.title)
                                .font(.tnCaption.weight(.medium))
                            if !item.subtitle.isEmpty {
                                Text(item.subtitle)
                                    .font(.tnMicro)
                                    .foregroundStyle(.secondary)
                                    .lineLimit(1)
                            }
                        }

                        Spacer()

                        Text(insertionPreview(item))
                            .font(.tnMicro.monospaced())
                            .foregroundStyle(.tertiary)
                    }
                    .padding(.horizontal, 10)
                    .padding(.vertical, 6)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(
                        index == highlightedIndex
                            ? Color.accentColor.opacity(0.14)
                            : Color.clear,
                        in: .rect(cornerRadius: TNCorner.sm)
                    )
                }
                .buttonStyle(.plain)
            }
        }
        .padding(6)
        .frame(width: 380)
        .background(.regularMaterial, in: .rect(cornerRadius: 12))
        .overlay(
            RoundedRectangle(cornerRadius: 12)
                .stroke(Color.tnBorder, lineWidth: 1)
        )
        .tnPopupShadow()
    }

    private func iconColor(for kind: CompletionItemKind) -> Color {
        switch kind {
        case .tag: .orange
        case .object: .teal
        case .reference: .accentColor
        }
    }

    private func insertionPreview(_ item: CompletionItem) -> String {
        switch item.kind {
        case .tag: item.insertionText
        case .object: "@\(item.insertionText)"
        case .reference: "[[\(item.insertionText)]]"
        }
    }
}

@MainActor
final class CompletionPanelController {
    private var panel: NSPanel?
    private(set) var items: [CompletionItem] = []
    private(set) var highlightedIndex = 0
    private var onChoose: ((CompletionItem) -> Void)?
    private var lastScreenRect: NSRect = .zero

    var isVisible: Bool { panel?.isVisible == true && !items.isEmpty }

    func show(items: [CompletionItem], highlightedIndex: Int, at screenRect: NSRect, onChoose: @escaping (CompletionItem) -> Void) {
        guard !items.isEmpty else {
            hide()
            return
        }

        self.items = items
        self.highlightedIndex = highlightedIndex
        self.onChoose = onChoose
        self.lastScreenRect = screenRect

        if panel == nil {
            let p = NSPanel(
                contentRect: .zero,
                styleMask: [.nonactivatingPanel],
                backing: .buffered,
                defer: true
            )
            p.isFloatingPanel = true
            p.level = .popUpMenu
            p.hasShadow = false
            p.backgroundColor = .clear
            p.isOpaque = false
            p.hidesOnDeactivate = true
            panel = p
        }

        refreshContent()
    }

    func hide() {
        panel?.orderOut(nil)
        items = []
        highlightedIndex = 0
        onChoose = nil
    }

    // MARK: - Keyboard navigation

    func moveUp() {
        guard !items.isEmpty else { return }
        highlightedIndex = (highlightedIndex - 1 + min(items.count, 6)) % min(items.count, 6)
        refreshContent()
    }

    func moveDown() {
        guard !items.isEmpty else { return }
        highlightedIndex = (highlightedIndex + 1) % min(items.count, 6)
        refreshContent()
    }

    func confirmSelection() {
        guard !items.isEmpty, highlightedIndex < items.count else { return }
        let chosen = items[highlightedIndex]
        onChoose?(chosen)
    }

    // MARK: - Internal

    private func refreshContent() {
        guard let panel else { return }

        let chooseHandler = onChoose
        let content = CompletionPopupContent(
            items: items,
            highlightedIndex: highlightedIndex,
            onChoose: { item in chooseHandler?(item) }
        )

        panel.contentView = NSHostingView(rootView: content)
        panel.contentView?.invalidateIntrinsicContentSize()

        let size = panel.contentView?.fittingSize ?? CGSize(width: 380, height: 200)
        let origin = CGPoint(
            x: lastScreenRect.origin.x,
            y: lastScreenRect.origin.y - size.height - 4
        )
        panel.setFrame(NSRect(origin: origin, size: size), display: true)
        panel.orderFront(nil)
    }
}
