import AppKit
import SwiftUI

struct TimelineSheet: View {
    let entries: [Entry]
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: TNSpacing.md) {
                HStack {
                    Text("Full Timeline")
                        .font(.tnPageTitle)
                    Spacer()
                    Button("Done") {
                        dismiss()
                    }
                    .buttonStyle(.bordered)
                }

                ForEach(entries) { entry in
                    VStack(alignment: .leading, spacing: TNSpacing.xs) {
                        HStack {
                            EntryKindBadge(kind: entry.kind)
                            Spacer()
                            Text(entry.createdAt, style: .relative)
                                .font(.tnMicro)
                                .foregroundStyle(.tertiary)
                        }
                        EntryBodyView(entry: entry)
                    }
                    .padding(.vertical, TNSpacing.sm)
                    ThinDivider()
                }
            }
            .padding(TNSpacing.lg)
        }
        .background(Color.tnBackground)
    }
}
