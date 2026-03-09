import AppKit
import SwiftUI

struct QuickCaptureView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.dismiss) private var dismiss
    @Environment(\.colorScheme) private var colorScheme
    @FocusState private var isFocused: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Quick Capture")
                .font(.title2.bold())

            Text("Write the thought. The system routes it automatically.")
                .foregroundStyle(.secondary)

            captureEditor

            HStack {
                Spacer()
                Button("Save") {
                    store.submitCapture()
                    dismiss()
                }
                .buttonStyle(.borderedProminent)
                .keyboardShortcut(.defaultAction)
            }
        }
        .padding(20)
        .frame(minWidth: 520, minHeight: 280)
        .background(
            LinearGradient(
                colors: [
                    Color(nsColor: .windowBackgroundColor),
                    Color(nsColor: colorScheme == .dark ? .underPageBackgroundColor : .controlBackgroundColor)
                ],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            )
        )
        .task {
            isFocused = true
        }
    }

    private var captureEditor: some View {
        TextEditor(text: $store.quickCaptureDraft.text)
            .font(.title3)
            .scrollContentBackground(.hidden)
            .padding(12)
            .frame(minHeight: 160)
            .background(.regularMaterial, in: .rect(cornerRadius: 18))
            .focused($isFocused)
    }
}
