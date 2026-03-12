import AppKit
import SwiftUI

struct RichCaptureEditor: NSViewRepresentable {
    @Binding var text: String
    var placeholder: String = "Type a note. Use #question, @object, or [[reference]]."
    var minHeight: CGFloat = 92
    var onSubmit: (() -> Void)?
    var completionProvider: (any CompletionProvider)?
    var panelController: CompletionPanelController?
    var onTriggerChanged: ((CompletionTrigger?, NSRect?) -> Void)?

    func makeNSView(context: Context) -> NSScrollView {
        let scrollView = NSScrollView()
        scrollView.hasVerticalScroller = true
        scrollView.hasHorizontalScroller = false
        scrollView.drawsBackground = false
        scrollView.borderType = .noBorder

        // Use TextKit1 explicitly so layoutManager is available for cursor positioning
        let textStorage = NSTextStorage()
        let layoutManager = NSLayoutManager()
        textStorage.addLayoutManager(layoutManager)
        let textContainer = NSTextContainer(containerSize: NSSize(width: 0, height: CGFloat.greatestFiniteMagnitude))
        textContainer.widthTracksTextView = true
        layoutManager.addTextContainer(textContainer)

        let textView = NSTextView(frame: .zero, textContainer: textContainer)
        textView.isRichText = false
        textView.allowsUndo = true
        textView.isEditable = true
        textView.isSelectable = true
        textView.drawsBackground = false
        textView.font = .systemFont(ofSize: 14)
        textView.textColor = .labelColor
        textView.insertionPointColor = .controlAccentColor
        textView.textContainerInset = NSSize(width: 12, height: 10)
        textView.isAutomaticQuoteSubstitutionEnabled = false
        textView.isAutomaticDashSubstitutionEnabled = false
        textView.isAutomaticTextReplacementEnabled = false
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.autoresizingMask = [.width]
        textView.delegate = context.coordinator

        scrollView.documentView = textView

        context.coordinator.textView = textView
        context.coordinator.scrollView = scrollView

        if !text.isEmpty {
            textView.string = text
            SyntaxHighlighter.highlight(textStorage: textView.textStorage!, editedRange: NSRange(location: 0, length: (text as NSString).length))
        }

        return scrollView
    }

    func updateNSView(_ scrollView: NSScrollView, context: Context) {
        guard let textView = scrollView.documentView as? NSTextView else { return }
        let coordinator = context.coordinator

        guard !coordinator.isUpdatingBinding else { return }

        if textView.string != text {
            coordinator.isSyncingFromBinding = true
            let selectedRanges = textView.selectedRanges
            textView.string = text
            SyntaxHighlighter.highlight(textStorage: textView.textStorage!, editedRange: NSRange(location: 0, length: (text as NSString).length))
            textView.selectedRanges = selectedRanges
            coordinator.isSyncingFromBinding = false
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(parent: self)
    }

    @MainActor
    final class Coordinator: NSObject, NSTextViewDelegate {
        let parent: RichCaptureEditor
        var isUpdatingBinding = false
        var isSyncingFromBinding = false
        weak var textView: NSTextView?
        weak var scrollView: NSScrollView?

        init(parent: RichCaptureEditor) {
            self.parent = parent
        }

        func textDidChange(_ notification: Notification) {
            guard let textView, !isSyncingFromBinding else { return }

            // Re-highlight after every edit
            SyntaxHighlighter.highlight(
                textStorage: textView.textStorage!,
                editedRange: NSRange(location: 0, length: (textView.string as NSString).length)
            )

            isUpdatingBinding = true
            parent.text = textView.string
            isUpdatingBinding = false

            detectTrigger()
        }

        func textView(_ textView: NSTextView, doCommandBy commandSelector: Selector) -> Bool {
            // When completion panel is visible, intercept navigation keys
            if let panel = parent.panelController, panel.isVisible {
                if commandSelector == #selector(NSResponder.moveUp(_:)) {
                    panel.moveUp()
                    return true
                }
                if commandSelector == #selector(NSResponder.moveDown(_:)) {
                    panel.moveDown()
                    return true
                }
                if commandSelector == #selector(NSResponder.insertNewline(_:)) {
                    // Plain Enter confirms completion selection
                    let event = NSApp.currentEvent
                    if event?.modifierFlags.contains(.command) == true {
                        // Cmd+Enter = submit the whole form
                        panel.hide()
                        parent.onTriggerChanged?(nil, nil)
                        parent.onSubmit?()
                        return true
                    }
                    panel.confirmSelection()
                    return true
                }
                if commandSelector == #selector(NSResponder.insertTab(_:)) {
                    panel.confirmSelection()
                    return true
                }
                if commandSelector == #selector(NSResponder.cancelOperation(_:)) {
                    panel.hide()
                    parent.onTriggerChanged?(nil, nil)
                    return true
                }
            } else {
                // No completion panel — Cmd+Enter submits
                if commandSelector == #selector(NSResponder.insertNewline(_:)) {
                    let event = NSApp.currentEvent
                    if event?.modifierFlags.contains(.command) == true {
                        parent.onSubmit?()
                        return true
                    }
                }
            }
            return false
        }

        // MARK: - Trigger Detection

        private func detectTrigger() {
            guard let textView else {
                print("[TN-COMPLETION] detectTrigger: no textView")
                parent.onTriggerChanged?(nil, nil)
                return
            }

            let string = textView.string as NSString
            let cursorLocation = textView.selectedRange().location
            print("[TN-COMPLETION] detectTrigger: cursor=\(cursorLocation), text='\(textView.string)'")

            guard cursorLocation > 0 else {
                print("[TN-COMPLETION] detectTrigger: cursor at 0, bail")
                parent.onTriggerChanged?(nil, nil)
                return
            }

            var tokenStart = cursorLocation
            while tokenStart > 0 {
                let char = string.substring(with: NSRange(location: tokenStart - 1, length: 1))
                if char.rangeOfCharacter(from: .whitespacesAndNewlines) != nil {
                    break
                }
                tokenStart -= 1
            }

            guard tokenStart < cursorLocation else {
                print("[TN-COMPLETION] detectTrigger: no token found")
                parent.onTriggerChanged?(nil, nil)
                return
            }

            let tokenRange = NSRange(location: tokenStart, length: cursorLocation - tokenStart)
            let token = string.substring(with: tokenRange)
            print("[TN-COMPLETION] detectTrigger: token='\(token)'")

            let trigger: CompletionTrigger?
            if token.hasPrefix("#") {
                let query = String(token.dropFirst()).lowercased()
                if let exactTag = CaptureTag(rawValue: query), exactTag.insertionText == token {
                    print("[TN-COMPLETION] exact tag match, no trigger")
                    trigger = nil
                } else {
                    trigger = .tag(query: query)
                }
            } else if token.hasPrefix("@") {
                let query = String(token.dropFirst()).lowercased()
                trigger = .object(query: query)
            } else if token.hasPrefix("[[") {
                let query = String(token.dropFirst(2)).lowercased()
                trigger = .reference(query: query)
            } else {
                trigger = nil
            }

            if let trigger {
                let cursorRect = cursorScreenRect(textView: textView)
                print("[TN-COMPLETION] trigger=\(trigger), cursorRect=\(cursorRect)")
                parent.onTriggerChanged?(trigger, cursorRect)
            } else {
                print("[TN-COMPLETION] no trigger matched")
                parent.onTriggerChanged?(nil, nil)
            }
        }

        private func cursorScreenRect(textView: NSTextView) -> NSRect {
            let insertionPoint = textView.selectedRange().location
            guard insertionPoint > 0 else {
                print("[TN-COMPLETION] cursorScreenRect: insertionPoint=0, returning .zero")
                return .zero
            }

            // Use firstRect(forCharacterRange:) — works with both TextKit1 and TextKit2
            var actualRange = NSRange()
            let charRange = NSRange(location: max(0, insertionPoint - 1), length: 1)
            let rect = textView.firstRect(forCharacterRange: charRange, actualRange: &actualRange)
            print("[TN-COMPLETION] cursorScreenRect: charRange=\(charRange), rect=\(rect), layoutManager=\(textView.layoutManager != nil)")

            guard rect != .zero else {
                print("[TN-COMPLETION] cursorScreenRect: rect is .zero")
                return .zero
            }

            return rect
        }

        func insertCompletion(_ text: String, replacingRange range: NSRange) {
            guard let textView else { return }
            isSyncingFromBinding = true
            textView.replaceCharacters(in: range, with: text + " ")
            isSyncingFromBinding = false
            isUpdatingBinding = true
            parent.text = textView.string
            isUpdatingBinding = false
            SyntaxHighlighter.highlight(textStorage: textView.textStorage!, editedRange: NSRange(location: 0, length: (textView.string as NSString).length))
            parent.onTriggerChanged?(nil, nil)
        }
    }
}
