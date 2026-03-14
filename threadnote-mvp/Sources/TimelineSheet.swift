import AppKit
import SwiftUI

struct TimelinePanel: View {
    let entries: [Entry]

    var body: some View {
        if entries.isEmpty {
            ContentUnavailableView(
                "No timeline yet",
                systemImage: "clock",
                description: Text("Notes will appear here as the thread moves.")
            )
            .frame(maxWidth: .infinity)
            .padding(.vertical, TNSpacing.xxl)
        } else {
            LazyVStack(alignment: .leading, spacing: TNSpacing.md) {
                TimelineLeadCard(entryCount: entries.count)

                ForEach(entries) { entry in
                    TimelineEntryCard(entry: entry)
                }
            }
        }
    }
}

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

                TimelinePanel(entries: entries)
            }
            .padding(TNSpacing.lg)
        }
        .background(Color.tnBackground)
    }
}

private struct TimelineLeadCard: View {
    let entryCount: Int

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            Text("Thread Timeline")
                .font(.tnSectionTitle)
            Text("Scan the sequence when you need chronology, then drop back into the working surface.")
                .font(.tnBody)
                .foregroundStyle(.secondary)

            Text("\(entryCount) entries")
                .font(.tnMicro.weight(.medium))
                .foregroundStyle(.secondary)
                .padding(.horizontal, TNSpacing.sm)
                .padding(.vertical, 6)
                .background(Color.tnSurface, in: Capsule())
        }
        .padding(TNSpacing.md)
        .background(Color.tnBackground.opacity(0.7), in: .rect(cornerRadius: TNCorner.lg))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.lg)
                .stroke(Color.tnBorderSubtle, lineWidth: 1)
        )
    }
}

private struct TimelineEntryCard: View {
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            HStack(alignment: .center, spacing: TNSpacing.xs) {
                Circle()
                    .fill(entry.kind.kindColor)
                    .frame(width: 6, height: 6)
                Spacer()
                Text(entry.createdAt.formatted(date: .abbreviated, time: .shortened))
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }

            NoteCard(entry: entry)
        }
        .padding(TNSpacing.md)
        .background(Color.tnBackground.opacity(0.55), in: .rect(cornerRadius: TNCorner.lg))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.lg)
                .stroke(Color.tnBorderSubtle, lineWidth: 1)
        )
    }
}
