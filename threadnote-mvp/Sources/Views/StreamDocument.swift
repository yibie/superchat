import SwiftUI

struct StreamDocument: View {
    @Environment(ThreadnoteStore.self) private var store
    @State private var draft = ""

    var body: some View {
        let provider = ThreadnoteCompletionProvider(store: store)

        VStack(alignment: .leading, spacing: 0) {
            Text("Stream")
                .font(.tnPageTitle)
                .padding(.bottom, TNSpacing.md)

            CaptureEditorView(
                text: $draft,
                helperText: "#role  @object  [[reference]]",
                submitLabel: "Send",
                minHeight: 120,
                submitAction: submitStreamCapture,
                completionProvider: provider
            )
            .padding(.bottom, TNSpacing.lg)

            LazyVStack(alignment: .leading, spacing: 0) {
                let groups = groupedByDate(store.streamEntries)
                let allEntries = groups.flatMap(\.entries)
                let lastRoot = allEntries.last(where: { $0.parentEntryID == nil })
                ForEach(groups, id: \.date) { group in
                    Text(group.label)
                        .font(.tnMicro.weight(.medium))
                        .foregroundStyle(.tertiary)
                        .padding(.top, TNSpacing.lg)
                        .padding(.bottom, TNSpacing.xs)
                        .padding(.leading, TNSpacing.md)

                    // Only root entries — replies rendered inside TimelineEntryRow
                    ForEach(group.entries.filter { $0.parentEntryID == nil }) { entry in
                        TimelineEntryRow(
                            entry: entry,
                            threadColor: nil,
                            isLast: entry.id == lastRoot?.id
                        )
                    }
                }
            }
        }
    }

    private func submitStreamCapture() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        store.quickCaptureDraft = QuickCaptureDraft(text: text)
        store.submitCapture()
        draft = ""
    }
}

// MARK: - Date grouping

private struct DateGroup {
    let date: String
    let label: String
    let entries: [Entry]
}

private func groupedByDate(_ entries: [Entry]) -> [DateGroup] {
    let calendar = Calendar.current
    let now = Date.now
    let today = calendar.startOfDay(for: now)
    let yesterday = calendar.date(byAdding: .day, value: -1, to: today)!

    let formatter = DateFormatter()
    formatter.dateFormat = "yyyy-MM-dd"

    let labelFormatter = DateFormatter()
    labelFormatter.dateStyle = .medium
    labelFormatter.timeStyle = .none

    var groups: [(key: String, label: String, entries: [Entry])] = []
    var currentKey = ""
    var currentEntries: [Entry] = []
    var currentLabel = ""

    for entry in entries {
        let entryDay = calendar.startOfDay(for: entry.createdAt)
        let key = formatter.string(from: entry.createdAt)

        let label: String
        if entryDay == today {
            label = "Today"
        } else if entryDay == yesterday {
            label = "Yesterday"
        } else {
            label = labelFormatter.string(from: entry.createdAt)
        }

        if key != currentKey {
            if !currentEntries.isEmpty {
                groups.append((key: currentKey, label: currentLabel, entries: currentEntries))
            }
            currentKey = key
            currentLabel = label
            currentEntries = [entry]
        } else {
            currentEntries.append(entry)
        }
    }

    if !currentEntries.isEmpty {
        groups.append((key: currentKey, label: currentLabel, entries: currentEntries))
    }

    return groups.map { DateGroup(date: $0.key, label: $0.label, entries: $0.entries) }
}
