import AppKit
import SwiftUI

private enum CaptureEditorCommand {
    case moveUp
    case moveDown
    case insertSelection
    case dismissSuggestions
    case submit
}

struct CaptureComposer: View {
    @Binding var text: String
    let helperText: String
    let submitLabel: String
    let minHeight: CGFloat
    let submitAction: () -> Void

    @State private var composerState = CaptureComposerState()
    @State private var selectedRange = NSRange(location: 0, length: 0)
    @State private var requestedSelectionRange: NSRange?
    @State private var focusRequestID = 0
    @State private var isComposingText = false

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            ZStack(alignment: .topLeading) {
                CaptureTextViewRepresentable(
                    text: $text,
                    selectedRange: $selectedRange,
                    requestedSelectionRange: $requestedSelectionRange,
                    isComposingText: $isComposingText,
                    focusRequestID: focusRequestID,
                    minHeight: minHeight,
                    onCommand: handleEditorCommand
                )
                .frame(minHeight: minHeight)
                .background(.thinMaterial, in: .rect(cornerRadius: 14))

                if text.isEmpty {
                    Text("Type a note. Use #question, #claim, #evidence, or #source.")
                        .font(.body)
                        .foregroundStyle(.tertiary)
                        .padding(.horizontal, 14)
                        .padding(.vertical, 12)
                        .allowsHitTesting(false)
                }

                if composerState.isShowingSuggestions && !isComposingText {
                    CaptureSuggestionList(
                        suggestions: composerState.suggestions,
                        highlightedIndex: composerState.highlightedSuggestionIndex,
                        onChoose: insertSuggestion
                    )
                    .padding(.top, 10)
                    .padding(.leading, 12)
                    .frame(maxWidth: 260, alignment: .leading)
                }
            }

            HStack(spacing: 10) {
                if let selectedTag = activeTag {
                    Label(selectedTag.displayText, systemImage: "number")
                        .font(.caption.weight(.semibold))
                        .padding(.horizontal, 10)
                        .padding(.vertical, 6)
                        .background(Color.accentColor.opacity(0.12), in: .capsule)
                } else {
                    Text(helperText)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Button(submitLabel) {
                    submitAction()
                }
                .buttonStyle(.borderedProminent)
                .disabled(text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .onAppear {
            requestFocus()
            recomputeSuggestions()
        }
        .onChange(of: text) { _, _ in
            guard !isComposingText else { return }
            recomputeSuggestions()
        }
        .onChange(of: selectedRange) { _, _ in
            guard !isComposingText else { return }
            recomputeSuggestions()
        }
        .onChange(of: isComposingText) { _, isComposing in
            if isComposing {
                composerState.insertionRange = nil
                composerState.suggestions = []
                composerState.activeQuery = ""
                composerState.highlightedSuggestionIndex = 0
            } else {
                recomputeSuggestions()
            }
        }
    }

    private var activeTag: CaptureTag? {
        parseActiveTag(in: text)
    }

    private func handleEditorCommand(_ command: CaptureEditorCommand) -> Bool {
        switch command {
        case .moveUp:
            guard composerState.isShowingSuggestions else { return false }
            composerState.highlightedSuggestionIndex = max(0, composerState.highlightedSuggestionIndex - 1)
            return true
        case .moveDown:
            guard composerState.isShowingSuggestions else { return false }
            composerState.highlightedSuggestionIndex = min(
                composerState.suggestions.count - 1,
                composerState.highlightedSuggestionIndex + 1
            )
            return true
        case .insertSelection:
            guard composerState.isShowingSuggestions,
                  composerState.suggestions.indices.contains(composerState.highlightedSuggestionIndex) else {
                return false
            }
            insertSuggestion(composerState.suggestions[composerState.highlightedSuggestionIndex].tag)
            return true
        case .dismissSuggestions:
            guard composerState.isShowingSuggestions else { return false }
            composerState.insertionRange = nil
            composerState.suggestions = []
            composerState.activeQuery = ""
            return true
        case .submit:
            submitAction()
            return true
        }
    }

    private func recomputeSuggestions() {
        let context = currentTagContext(in: text, selectedRange: selectedRange)
        composerState.insertionRange = context?.range
        composerState.activeQuery = context?.query ?? ""
        composerState.selectedTag = parseActiveTag(in: text)

        guard let context else {
            composerState.suggestions = []
            composerState.highlightedSuggestionIndex = 0
            return
        }

        let query = context.query.lowercased()
        let suggestions = CaptureTag.allCases
            .filter { query.isEmpty || $0.rawValue.hasPrefix(query) }
            .map(CaptureSuggestion.init)

        composerState.suggestions = suggestions
        composerState.highlightedSuggestionIndex = min(composerState.highlightedSuggestionIndex, max(0, suggestions.count - 1))
    }

    private func insertSuggestion(_ tag: CaptureTag) {
        guard let range = composerState.insertionRange else { return }
        let nsText = text as NSString
        let insertion = "\(tag.insertionText) "
        let updated = nsText.replacingCharacters(in: range, with: insertion)
        text = updated
        let nextSelection = NSRange(location: range.location + (insertion as NSString).length, length: 0)
        selectedRange = nextSelection
        requestedSelectionRange = nextSelection
        composerState.selectedTag = tag
        composerState.insertionRange = nil
        composerState.activeQuery = ""
        composerState.suggestions = []
        composerState.highlightedSuggestionIndex = 0
        requestFocus()
    }

    private func currentTagContext(in text: String, selectedRange: NSRange) -> (query: String, range: NSRange)? {
        let nsText = text as NSString
        let cursorLocation = min(selectedRange.location, nsText.length)

        if cursorLocation < nsText.length {
            let nextCharacter = nsText.substring(with: NSRange(location: cursorLocation, length: 1))
            if nextCharacter.rangeOfCharacter(from: .whitespacesAndNewlines) != nil {
                return nil
            }
        }

        let prefix = nsText.substring(to: cursorLocation)
        guard let hashIndex = prefix.lastIndex(of: "#") else { return nil }

        let tokenStart = prefix.distance(from: prefix.startIndex, to: hashIndex)
        let token = String(prefix[hashIndex...])
        guard !token.contains(where: \.isWhitespace) else { return nil }
        guard token.count >= 1 else { return nil }

        let query = String(token.dropFirst()).lowercased()
        if let exactTag = CaptureTag(rawValue: query) {
            let tokenRange = NSRange(location: tokenStart, length: token.utf16.count)
            let tokenEnd = tokenRange.location + tokenRange.length
            if tokenEnd < nsText.length {
                let followingCharacter = nsText.substring(with: NSRange(location: tokenEnd, length: 1))
                if followingCharacter.rangeOfCharacter(from: .whitespacesAndNewlines) != nil {
                    return nil
                }
            } else if exactTag.insertionText == token {
                return nil
            }
        }

        let range = NSRange(location: tokenStart, length: token.utf16.count)
        return (query, range)
    }

    private func parseActiveTag(in text: String) -> CaptureTag? {
        let supportedTags = CaptureTag.allCases.map(\.rawValue).joined(separator: "|")
        guard let regex = try? NSRegularExpression(pattern: #"(?<!\S)#("# + supportedTags + #")\b"#, options: [.caseInsensitive]) else {
            return nil
        }
        let nsRange = NSRange(text.startIndex..<text.endIndex, in: text)
        guard let match = regex.firstMatch(in: text, options: [], range: nsRange),
              let range = Range(match.range(at: 1), in: text) else {
            return nil
        }
        return CaptureTag(rawValue: text[range].lowercased())
    }

    private func requestFocus() {
        focusRequestID += 1
    }
}

private struct CaptureSuggestionList: View {
    let suggestions: [CaptureSuggestion]
    let highlightedIndex: Int
    let onChoose: (CaptureTag) -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            ForEach(Array(suggestions.enumerated()), id: \.element.id) { index, suggestion in
                Button {
                    onChoose(suggestion.tag)
                } label: {
                    HStack {
                        Text(suggestion.tag.displayText)
                        Spacer()
                        Text(suggestion.tag.entryKind.title)
                            .foregroundStyle(.secondary)
                    }
                    .font(.caption)
                    .padding(.horizontal, 10)
                    .padding(.vertical, 8)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(index == highlightedIndex ? Color.accentColor.opacity(0.14) : Color.clear, in: .rect(cornerRadius: 8))
                }
                .buttonStyle(.plain)
            }
        }
        .padding(8)
        .background(.regularMaterial, in: .rect(cornerRadius: 12))
        .overlay(
            RoundedRectangle(cornerRadius: 12)
                .stroke(Color.primary.opacity(0.06), lineWidth: 1)
        )
        .shadow(color: .black.opacity(0.08), radius: 12, y: 6)
    }
}

private struct CaptureTextViewRepresentable: NSViewRepresentable {
    @Binding var text: String
    @Binding var selectedRange: NSRange
    @Binding var requestedSelectionRange: NSRange?
    @Binding var isComposingText: Bool

    let focusRequestID: Int
    let minHeight: CGFloat
    let onCommand: (CaptureEditorCommand) -> Bool

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeNSView(context: Context) -> CaptureEditorContainerView {
        let view = CaptureEditorContainerView()
        context.coordinator.attach(to: view)
        view.configure(minHeight: minHeight)
        view.textView.delegate = context.coordinator
        view.textView.commandHandler = onCommand
        view.textView.string = text
        return view
    }

    func updateNSView(_ nsView: CaptureEditorContainerView, context: Context) {
        context.coordinator.parent = self
        nsView.configure(minHeight: minHeight)
        nsView.textView.commandHandler = onCommand

        let composing = nsView.textView.hasMarkedText()
        if isComposingText != composing {
            DispatchQueue.main.async {
                self.isComposingText = composing
            }
        }

        guard !composing else { return }

        if nsView.textView.string != text {
            nsView.textView.string = text
        }

        if let requestedSelectionRange, nsView.textView.selectedRange() != requestedSelectionRange {
            context.coordinator.isApplyingSelectionUpdate = true
            nsView.textView.setSelectedRange(requestedSelectionRange)
            context.coordinator.isApplyingSelectionUpdate = false
            DispatchQueue.main.async {
                self.requestedSelectionRange = nil
            }
        }

        nsView.applyFocusRequest(focusRequestID)
    }

    final class Coordinator: NSObject, NSTextViewDelegate {
        var parent: CaptureTextViewRepresentable
        weak var containerView: CaptureEditorContainerView?
        var isApplyingSelectionUpdate = false

        init(_ parent: CaptureTextViewRepresentable) {
            self.parent = parent
        }

        func attach(to view: CaptureEditorContainerView) {
            containerView = view
        }

        func textDidChange(_ notification: Notification) {
            guard let textView = notification.object as? NSTextView else { return }
            let composing = textView.hasMarkedText()
            if parent.isComposingText != composing {
                parent.isComposingText = composing
            }
            guard !composing else { return }
            let updated = textView.string
            if parent.text != updated {
                parent.text = updated
            }
            if let currentRange = textView.selectedRanges.first?.rangeValue, parent.selectedRange != currentRange {
                parent.selectedRange = currentRange
            }
        }

        func textViewDidChangeSelection(_ notification: Notification) {
            guard let textView = notification.object as? NSTextView,
                  let currentRange = textView.selectedRanges.first?.rangeValue else {
                return
            }
            guard !isApplyingSelectionUpdate else { return }
            let composing = textView.hasMarkedText()
            if parent.isComposingText != composing {
                parent.isComposingText = composing
            }
            guard !composing else { return }
            if parent.selectedRange != currentRange {
                parent.selectedRange = currentRange
            }
        }
    }
}

private final class CaptureEditorContainerView: NSView {
    let scrollView = NSScrollView()
    let textView = CaptureTextView(frame: .zero)

    private var lastFocusRequestID = -1

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setup()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }

    override var acceptsFirstResponder: Bool { true }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if lastFocusRequestID >= 0 {
            DispatchQueue.main.async { [weak self] in
                self?.focusTextView()
            }
        }
    }

    func configure(minHeight: CGFloat) {
        textView.minSize = NSSize(width: 0, height: minHeight)
    }

    func applyFocusRequest(_ focusRequestID: Int) {
        guard focusRequestID != lastFocusRequestID else { return }
        lastFocusRequestID = focusRequestID
        DispatchQueue.main.async { [weak self] in
            self?.focusTextView()
        }
    }

    private func setup() {
        wantsLayer = true

        scrollView.translatesAutoresizingMaskIntoConstraints = false
        scrollView.drawsBackground = false
        scrollView.borderType = .noBorder
        scrollView.hasVerticalScroller = true
        scrollView.hasHorizontalScroller = false
        scrollView.autohidesScrollers = true
        scrollView.scrollerStyle = .overlay

        let textContainer = textView.textContainer
        textContainer?.containerSize = NSSize(width: 0, height: CGFloat.greatestFiniteMagnitude)
        textContainer?.widthTracksTextView = true

        textView.commandHandler = nil
        textView.font = .preferredFont(forTextStyle: .body)
        textView.textColor = .labelColor
        textView.insertionPointColor = .labelColor
        textView.isRichText = false
        textView.isEditable = true
        textView.isSelectable = true
        textView.importsGraphics = false
        textView.isAutomaticQuoteSubstitutionEnabled = false
        textView.isAutomaticDataDetectionEnabled = false
        textView.allowsUndo = true
        textView.drawsBackground = false
        textView.textContainerInset = NSSize(width: 10, height: 10)
        textView.maxSize = NSSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.autoresizingMask = [.width]

        scrollView.documentView = textView
        addSubview(scrollView)

        NSLayoutConstraint.activate([
            scrollView.leadingAnchor.constraint(equalTo: leadingAnchor),
            scrollView.trailingAnchor.constraint(equalTo: trailingAnchor),
            scrollView.topAnchor.constraint(equalTo: topAnchor),
            scrollView.bottomAnchor.constraint(equalTo: bottomAnchor)
        ])
    }

    private func focusTextView() {
        guard let window else { return }
        if !window.isKeyWindow {
            window.makeKeyAndOrderFront(nil)
        }
        window.makeFirstResponder(textView)
    }
}

private final class CaptureTextView: NSTextView {
    var commandHandler: ((CaptureEditorCommand) -> Bool)?

    override var acceptsFirstResponder: Bool { true }

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool {
        true
    }

    override func mouseDown(with event: NSEvent) {
        window?.makeFirstResponder(self)
        super.mouseDown(with: event)
    }

    override func becomeFirstResponder() -> Bool {
        let didBecome = super.becomeFirstResponder()
        if didBecome {
            needsDisplay = true
        }
        return didBecome
    }

    override func keyDown(with event: NSEvent) {
        if hasMarkedText() {
            super.keyDown(with: event)
            return
        }

        if event.modifierFlags.contains(.command),
           (event.keyCode == 36 || event.keyCode == 76),
           commandHandler?(.submit) == true {
            return
        }

        let handled: Bool
        switch event.keyCode {
        case 126:
            handled = commandHandler?(.moveUp) == true
        case 125:
            handled = commandHandler?(.moveDown) == true
        case 36, 48, 76:
            handled = commandHandler?(.insertSelection) == true
        case 53:
            handled = commandHandler?(.dismissSuggestions) == true
        default:
            handled = false
        }

        if handled {
            return
        }

        super.keyDown(with: event)
    }
}
