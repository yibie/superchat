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
    /// Called when the user drops a file. Return a path string to insert (e.g. relative attachment path).
    var onFileDrop: ((URL) -> String?)?

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

        let textView = CaptureTextView(frame: .zero, textContainer: textContainer)
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
        textView.registerDragTypes()
        let coordinator = context.coordinator
        textView.onCmdReturn = { [weak coordinator] in
            coordinator?.parent.onSubmit?()
        }

        scrollView.documentView = textView

        context.coordinator.textView = textView
        context.coordinator.scrollView = scrollView

        DispatchQueue.main.async {
            textView.window?.makeFirstResponder(textView)
        }

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

        (textView as? CaptureTextView)?.onFileDrop = onFileDrop

        if textView.string != text {
            coordinator.isSyncingFromBinding = true
            textView.string = text
            SyntaxHighlighter.highlight(textStorage: textView.textStorage!, editedRange: NSRange(location: 0, length: (text as NSString).length))
            // Place cursor at end of text — safe default after binding-driven changes
            let endPos = (text as NSString).length
            textView.setSelectedRange(NSRange(location: endPos, length: 0))
            coordinator.isSyncingFromBinding = false
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(parent: self)
    }

    @MainActor
    final class Coordinator: NSObject, NSTextViewDelegate {
        var parent: RichCaptureEditor
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
            }
            return false
        }

        // MARK: - Trigger Detection

        private func detectTrigger() {
            guard let textView else {
                parent.onTriggerChanged?(nil, nil)
                return
            }

            let string = textView.string as NSString
            let cursorLocation = textView.selectedRange().location

            guard cursorLocation > 0 else {
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
                parent.onTriggerChanged?(nil, nil)
                return
            }

            let tokenRange = NSRange(location: tokenStart, length: cursorLocation - tokenStart)
            let token = string.substring(with: tokenRange)

            let trigger: CompletionTrigger?
            if token.hasPrefix("#") {
                let query = String(token.dropFirst()).lowercased()
                if let exactTag = CaptureTag(rawValue: query), exactTag.insertionText == token {
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
                parent.onTriggerChanged?(trigger, cursorRect)
            } else {
                parent.onTriggerChanged?(nil, nil)
            }
        }

        private func cursorScreenRect(textView: NSTextView) -> NSRect {
            let insertionPoint = textView.selectedRange().location
            guard insertionPoint > 0 else {
                return .zero
            }

            var actualRange = NSRange()
            let charRange = NSRange(location: max(0, insertionPoint - 1), length: 1)
            let rect = textView.firstRect(forCharacterRange: charRange, actualRange: &actualRange)

            guard rect != .zero else {
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

// MARK: - CaptureTextView (intercepts Cmd+Return at the key equivalent level)

@MainActor
final class CaptureTextView: NSTextView {
    var onCmdReturn: (() -> Void)?
    /// Called for each dropped file URL. Return a path string to insert, or nil to fall back to file://.
    var onFileDrop: ((URL) -> String?)?

    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        if event.modifierFlags.contains(.command),
           event.keyCode == 36 /* Return */ {
            onCmdReturn?()
            return true
        }
        return super.performKeyEquivalent(with: event)
    }

    // MARK: - File drag & drop

    override func registerForDraggedTypes(_ newTypes: [NSPasteboard.PasteboardType]) {
        super.registerForDraggedTypes(newTypes)
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        registerDragTypes()
    }

    func registerDragTypes() {
        registerForDraggedTypes([.fileURL, .URL, .string])
    }

    override func draggingEntered(_ sender: NSDraggingInfo) -> NSDragOperation {
        let pb = sender.draggingPasteboard
        if pb.canReadItem(withDataConformingToTypes: [NSPasteboard.PasteboardType.fileURL.rawValue]) {
            return .copy
        }
        return super.draggingEntered(sender)
    }

    override func performDragOperation(_ sender: NSDraggingInfo) -> Bool {
        let pb = sender.draggingPasteboard
        if let urls = pb.readObjects(forClasses: [NSURL.self], options: [.urlReadingFileURLsOnly: true]) as? [URL], !urls.isEmpty {
            let insertion = urls.map { url in
                onFileDrop?(url) ?? url.absoluteString
            }.joined(separator: "\n")
            let range = selectedRange()
            if shouldChangeText(in: range, replacementString: insertion) {
                replaceCharacters(in: range, with: insertion)
                didChangeText()
            }
            return true
        }
        return super.performDragOperation(sender)
    }
}
