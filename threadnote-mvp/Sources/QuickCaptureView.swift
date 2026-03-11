import AppKit
import SwiftUI

struct QuickCaptureView: View {
    @Bindable var store: ThreadnoteStore
    @Environment(\.dismiss) private var dismiss
    @Environment(\.colorScheme) private var colorScheme

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Quick Capture")
                .font(.title2.bold())

            Text("Write the note. Use `#role` when you want to set its role explicitly, for example `#question`, `#claim`, `#evidence`, or `#decided`.")
                .foregroundStyle(.secondary)

            CaptureComposer(
                text: $store.quickCaptureDraft.text,
                helperText: "Quick capture uses the same `#role`, `@object`, and `[[reference]]` syntax as the main stream.",
                submitLabel: "Save",
                minHeight: 180
            ) {
                let trimmed = store.quickCaptureDraft.text.trimmingCharacters(in: .whitespacesAndNewlines)
                guard !trimmed.isEmpty else { return }
                store.submitCapture()
                dismiss()
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
    }
}
