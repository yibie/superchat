import AppKit
import SwiftUI

// MARK: - NewThreadSheet

struct NewThreadSheet: View {
    @Environment(ThreadnoteStore.self) private var store
    let context: ThreadCreationContext

    @State private var title: String
    @State private var goalType: ThreadGoalType

    init(context: ThreadCreationContext) {
        self.context = context
        _title = State(initialValue: context.suggestedTitle)
        _goalType = State(initialValue: context.suggestedGoalType)
    }

    private var trimmedTitle: String {
        title.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.lg) {
            Text(context.editingThreadID == nil ? "New Thread" : "Edit Thread")
                .font(.tnPageTitle)

            Text("Name the thread, then choose the kind of work it belongs to.")
                .font(.tnCaption)
                .foregroundStyle(.secondary)

            VStack(alignment: .leading, spacing: TNSpacing.md) {
                Text("Title")
                    .font(.tnSectionTitle)
                TextField("Short thread title", text: $title)
                    .textFieldStyle(.roundedBorder)

                Text("Category")
                    .font(.tnSectionTitle)
                Picker("Category", selection: $goalType) {
                    ForEach(ThreadGoalType.allCases) { option in
                        Label(option.title, systemImage: option.systemImage)
                            .tag(option)
                    }
                }
                .pickerStyle(.segmented)
            }

            Spacer()

            HStack {
                Spacer()
                Button("Cancel") {
                    store.dismissThreadCreation()
                }
                .buttonStyle(.bordered)

                Button(context.editingThreadID == nil ? "Create Thread" : "Save Thread") {
                    store.completeThreadCreation(
                        title: title,
                        goalType: goalType
                    )
                }
                .buttonStyle(.borderedProminent)
                .disabled(trimmedTitle.isEmpty)
            }
        }
        .padding(TNSpacing.lg)
        .background(Color.tnBackground)
    }
}
