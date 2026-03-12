import SwiftUI

struct CaptureEditorView: View {
    @Binding var text: String
    let helperText: String
    let submitLabel: String
    let minHeight: CGFloat
    let submitAction: () -> Void
    var completionProvider: (any CompletionProvider)?

    @State private var composerState = CaptureComposerState()
    @State private var panelController = CompletionPanelController()
    @State private var activeTrigger: CompletionTrigger?
    @State private var triggerScreenRect: NSRect?
    @State private var completionItems: [CompletionItem] = []
    @State private var highlightedIndex = 0

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            RichCaptureEditor(
                text: $text,
                placeholder: helperText,
                minHeight: minHeight,
                onSubmit: submitAction,
                completionProvider: completionProvider,
                panelController: panelController,
                onTriggerChanged: { trigger, rect in
                    activeTrigger = trigger
                    triggerScreenRect = rect
                    updateCompletions(trigger: trigger, rect: rect)
                }
            )
            .frame(minHeight: minHeight)
            .background(Color.tnSurface.opacity(0.5), in: .rect(cornerRadius: TNCorner.md))
            .overlay(
                RoundedRectangle(cornerRadius: TNCorner.md)
                    .stroke(Color.tnBorder, lineWidth: 1)
            )

            HStack(spacing: TNSpacing.sm) {
                if let selectedTag = composerState.selectedTag {
                    HStack(spacing: 4) {
                        Circle()
                            .fill(selectedTag.entryKind.kindColor)
                            .frame(width: 6, height: 6)
                        Text(selectedTag.displayText)
                            .font(.tnCaption.weight(.medium))
                    }
                    .padding(.horizontal, 8)
                    .padding(.vertical, 4)
                    .background(selectedTag.entryKind.kindColor.opacity(0.1), in: .capsule)
                } else {
                    Text(helperText)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Text("⌘↩ to send")
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)

                Button(submitLabel) {
                    submitAction()
                }
                .buttonStyle(.borderedProminent)
                .controlSize(.small)
                .disabled(text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .onChange(of: text) { _, _ in
            composerState.selectedTag = CaptureTag.parseTag(in: text)
        }
        .onDisappear {
            panelController.hide()
        }
    }

    private func updateCompletions(trigger: CompletionTrigger?, rect: NSRect?) {
        print("[TN-EDITOR] updateCompletions: trigger=\(String(describing: trigger)), rect=\(String(describing: rect)), provider=\(completionProvider != nil)")

        guard let trigger, let rect, let provider = completionProvider else {
            print("[TN-EDITOR] updateCompletions: guard failed — trigger=\(trigger != nil), rect=\(rect != nil), provider=\(completionProvider != nil)")
            completionItems = []
            highlightedIndex = 0
            panelController.hide()
            return
        }

        let items = provider.completions(for: trigger)
        completionItems = items
        highlightedIndex = 0
        print("[TN-EDITOR] updateCompletions: \(items.count) items for trigger=\(trigger)")

        if items.isEmpty {
            panelController.hide()
        } else {
            print("[TN-EDITOR] showing panel at \(rect) with \(items.count) items")
            panelController.show(
                items: items,
                highlightedIndex: highlightedIndex,
                at: rect
            ) { chosen in
                insertCompletion(chosen, trigger: trigger)
            }
        }
    }

    private func insertCompletion(_ item: CompletionItem, trigger: CompletionTrigger) {
        let nsText = text as NSString
        let length = nsText.length

        // Find the trigger token to replace
        var tokenStart = length
        while tokenStart > 0 {
            let char = nsText.substring(with: NSRange(location: tokenStart - 1, length: 1))
            if char.rangeOfCharacter(from: .whitespacesAndNewlines) != nil {
                break
            }
            tokenStart -= 1
        }

        let range = NSRange(location: tokenStart, length: length - tokenStart)
        let insertion: String
        switch trigger {
        case .tag:
            insertion = item.insertionText
        case .object:
            insertion = "@\(item.insertionText)"
        case .reference:
            insertion = "[[\(item.insertionText)]]"
        }

        text = nsText.replacingCharacters(in: range, with: insertion + " ")
        panelController.hide()
        completionItems = []
    }
}
