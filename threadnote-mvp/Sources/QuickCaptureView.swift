import AppKit
import SwiftUI

struct QuickCaptureView: View {
    @Environment(ThreadnoteStore.self) private var store
    @Environment(\.dismiss) private var dismiss
    @State private var completionProvider: ThreadnoteCompletionProvider?

    var body: some View {
        @Bindable var store = store
        VStack(alignment: .leading, spacing: TNSpacing.md) {
            Text("Quick Capture")
                .font(.tnPageTitle)

            Text("Use #role, @object, and [[reference]] to shape the note.")
                .font(.tnCaption)
                .foregroundStyle(.secondary)

            CaptureEditorView(
                text: $store.quickCaptureDraft.text,
                helperText: "#role  @object  [[reference]]",
                submitLabel: "Save",
                minHeight: 160,
                submitAction: {
                    let trimmed = store.quickCaptureDraft.text.trimmingCharacters(in: .whitespacesAndNewlines)
                    guard !trimmed.isEmpty else { return }
                    store.submitCapture()
                    dismiss()
                },
                completionProvider: completionProvider
            )
        }
        .padding(TNSpacing.lg)
        .frame(minWidth: 520, minHeight: 280)
        .background(Color.tnBackground)
        .onAppear {
            if completionProvider == nil {
                completionProvider = ThreadnoteCompletionProvider(store: store)
            }
        }
    }
}
