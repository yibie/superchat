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

// MARK: - NewListSheet

struct NewListSheet: View {
    @Environment(ThreadnoteStore.self) private var store
    @Binding var isPresented: Bool

    @State private var title = ""
    @State private var description = ""
    @State private var kind: ListKind = .pack

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.lg) {
            Text("New List")
                .font(.tnPageTitle)

            Text("Create a lightweight resource area for threads, notes, and sources.")
                .font(.tnCaption)
                .foregroundStyle(.secondary)

            VStack(alignment: .leading, spacing: TNSpacing.md) {
                Text("Title")
                    .font(.tnSectionTitle)
                TextField("Reading Queue", text: $title)
                    .textFieldStyle(.roundedBorder)

                Text("Description")
                    .font(.tnSectionTitle)
                TextField("What this list is organizing", text: $description)
                    .textFieldStyle(.roundedBorder)

                Text("Kind")
                    .font(.tnSectionTitle)
                Picker("Kind", selection: $kind) {
                    ForEach(ListKind.allCases) { option in
                        Text(option.title).tag(option)
                    }
                }
                .pickerStyle(.segmented)
            }

            Spacer()

            HStack {
                Spacer()
                Button("Cancel") {
                    isPresented = false
                }
                .buttonStyle(.bordered)

                Button("Create List") {
                    let createdID = store.createList(
                        title: title.trimmingCharacters(in: .whitespacesAndNewlines),
                        description: description.trimmingCharacters(in: .whitespacesAndNewlines),
                        kind: kind
                    )
                    store.selectList(createdID)
                    isPresented = false
                }
                .buttonStyle(.borderedProminent)
                .disabled(title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }
        }
        .padding(TNSpacing.lg)
        .background(Color.tnBackground)
    }
}
