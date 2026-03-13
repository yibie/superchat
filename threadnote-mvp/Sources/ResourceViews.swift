import AppKit
import SwiftUI

struct ResourceOverviewRow: View {
    let counts: ResourceCounts

    var body: some View {
        HStack(spacing: TNSpacing.sm) {
            ForEach(ResourceKind.allCases) { kind in
                ResourceMetricPill(kind: kind, count: counts[kind])
            }
            Spacer(minLength: 0)
        }
    }
}

struct ResourcesDocument: View {
    @Environment(ThreadnoteStore.self) private var store

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            Text("Resources")
                .font(.tnPageTitle)

            Text("All thread resources derived from links, media, and @object mentions.")
                .font(.tnBody)
                .foregroundStyle(.secondary)
                .padding(.top, TNSpacing.xs)

            ResourceOverviewRow(counts: store.allResourceCounts)
                .padding(.vertical, TNSpacing.md)

            ThinDivider()

            if store.allResources.isEmpty {
                ContentUnavailableView(
                    "No resources yet",
                    systemImage: "books.vertical",
                    description: Text("Resources appear when notes include links, media-like attachments, or @object mentions.")
                )
                .frame(maxWidth: .infinity)
                .padding(.vertical, TNSpacing.xxl)
            } else {
                ResourceTypedSections(
                    resources: store.allResources,
                    previewLimit: nil,
                    showThreadLabel: true
                )
            }
        }
    }
}

struct SourceDetailSheet: View {
    @Environment(ThreadnoteStore.self) private var store
    let entry: Entry

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.lg) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: TNSpacing.sm) {
                    Label("Source", systemImage: "bookmark")
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.secondary)
                    Text(entry.sourceDisplayTitle)
                        .font(.tnPageTitle)
                }

                Spacer()

                Button("Done") {
                    store.closeSource()
                }
                .buttonStyle(.bordered)
            }

            if let locator = entry.sourceLocator {
                DocumentSection(title: "Locator", systemImage: "link") {
                    Text(locator)
                        .textSelection(.enabled)
                        .font(.tnBody)

                    if let url = URL(string: locator), let scheme = url.scheme {
                        Button(scheme.hasPrefix("http") ? "Open in Browser" : "Open Resource", systemImage: "arrow.up.right.square") {
                            NSWorkspace.shared.open(url)
                        }
                        .buttonStyle(.borderedProminent)
                        .controlSize(.small)
                    }
                }
            }

            DocumentSection(title: "Summary", systemImage: "text.justify") {
                RichBodyView(entry: entry)
                if let citation = entry.sourceMetadata?.citation, !citation.isEmpty {
                    ThinDivider()
                    Text(citation)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }
            }

            if let thread = store.thread(for: entry) {
                DocumentSection(title: "Problem Space", systemImage: "square.stack.3d.up") {
                    VStack(alignment: .leading, spacing: TNSpacing.sm) {
                        Text(thread.title)
                            .font(.tnSectionTitle)
                        if let subtitle = threadSecondarySummary(thread) {
                            Text(subtitle)
                                .font(.tnBody)
                                .foregroundStyle(.secondary)
                        }
                        Button("Open Thread") {
                            store.openThread(thread.id)
                            store.closeSource()
                        }
                        .buttonStyle(.bordered)
                        .controlSize(.small)
                    }
                }
            }

            Spacer(minLength: 0)
        }
        .padding(TNSpacing.lg)
        .background(Color.tnBackground)
    }
}

struct ThreadResourcesPanel: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord

    var body: some View {
        let counts = store.resourceCounts(for: thread.id)

        VStack(alignment: .leading, spacing: TNSpacing.lg) {
            ResourceInspectorLeadCard(counts: counts)
            ResourceTypedSections(
                resources: store.resourceItems(for: thread.id),
                previewLimit: nil,
                showThreadLabel: false
            )
        }
    }
}

private struct ResourceThreadSection: View {
    @Environment(ThreadnoteStore.self) private var store
    let thread: ThreadRecord
    let previewLimit: Int?
    let showsOpenThreadButton: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.md) {
            HStack(alignment: .center, spacing: TNSpacing.sm) {
                VStack(alignment: .leading, spacing: TNSpacing.xs) {
                    Text(thread.title)
                        .font(.tnSectionTitle)
                    if let subtitle = threadSecondarySummary(thread) {
                        Text(subtitle)
                            .font(.tnCaption)
                            .foregroundStyle(.secondary)
                    }
                }

                Spacer()

                if showsOpenThreadButton {
                    Button("Open Thread") {
                        store.openThread(thread.id)
                    }
                    .buttonStyle(.bordered)
                    .controlSize(.small)
                }
            }

            ResourceOverviewRow(counts: store.resourceCounts(for: thread.id))

            ResourceTypedSections(
                resources: store.resourceItems(for: thread.id),
                previewLimit: previewLimit,
                showThreadLabel: false
            )
        }
    }
}

private struct ResourceTypedSections: View {
    let resources: [ResourceItem]
    let previewLimit: Int?
    let showThreadLabel: Bool

    var body: some View {
        let sections = ResourceKind.allCases.compactMap { kind -> (kind: ResourceKind, items: [ResourceItem])? in
            let items = resources.filter { $0.kind == kind }
            guard !items.isEmpty else { return nil }
            if let previewLimit {
                return (kind, Array(items.prefix(previewLimit)))
            }
            return (kind, items)
        }

        if sections.isEmpty {
            Text("No resources yet.")
                .font(.tnCaption)
                .foregroundStyle(.tertiary)
        } else {
            VStack(alignment: .leading, spacing: TNSpacing.md) {
                ForEach(sections, id: \.kind) { section in
                    ResourceKindSection(
                        kind: section.kind,
                        items: section.items,
                        showThreadLabel: showThreadLabel
                    )
                }
            }
        }
    }
}

private struct ResourceKindSection: View {
    let kind: ResourceKind
    let items: [ResourceItem]
    let showThreadLabel: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.md) {
            HStack(alignment: .top, spacing: TNSpacing.sm) {
                VStack(alignment: .leading, spacing: TNSpacing.xs) {
                    Label(kind.title, systemImage: kind.systemImage)
                        .font(.tnCaption.weight(.medium))
                        .foregroundStyle(.secondary)
                    Text(kind.panelDescription)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                }

                Spacer()

                Text("\(items.count)")
                    .font(.tnMicro.weight(.semibold))
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, TNSpacing.sm)
                    .padding(.vertical, 6)
                    .background(Color.tnSurface, in: Capsule())
            }

            VStack(alignment: .leading, spacing: 0) {
                ForEach(Array(items.enumerated()), id: \.element.id) { idx, resource in
                    ResourceEntryRow(resource: resource, showThreadLabel: showThreadLabel)
                    if idx < items.count - 1 {
                        ThinDivider().padding(.vertical, TNSpacing.xs)
                    }
                }
            }
        }
    }
}

private struct ResourceEntryRow: View {
    @Environment(ThreadnoteStore.self) private var store
    let resource: ResourceItem
    let showThreadLabel: Bool

    private var entry: Entry { resource.entry }
    private var thread: ThreadRecord? { store.thread(for: entry) }

    // For URL entries: previewText is the raw URL — skip it.
    // Show details text only if it adds context beyond the title.
    private var detailsText: String? {
        // entry.body.details is the most useful human annotation
        if let d = entry.body.details, !d.isEmpty { return d }
        // citation is a good fallback for sources
        if let c = entry.sourceMetadata?.citation, !c.isEmpty { return c }
        return nil
    }

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.sm) {
            // Header: timestamp only — kind is implied by the content below
            HStack(alignment: .firstTextBaseline) {
                if showThreadLabel, let thread {
                    Button {
                        store.openThread(thread.id)
                    } label: {
                        HStack(spacing: 3) {
                            Circle()
                                .fill(thread.color.color)
                                .frame(width: 6, height: 6)
                            Text(thread.title)
                                .lineLimit(1)
                        }
                        .font(.tnMicro)
                        .foregroundStyle(.secondary)
                    }
                    .buttonStyle(.plain)
                }
                Spacer()
                Text(entry.createdAt, format: .dateTime.month(.abbreviated).day().hour().minute())
                    .font(.tnMicro)
                    .foregroundStyle(.tertiary)
            }

            // Title — only if it's not just a raw URL
            let isRawURL = resource.title.hasPrefix("http")
            if !isRawURL {
                Text(resource.title)
                    .font(.tnSectionTitle)
                    .fixedSize(horizontal: false, vertical: true)
            }

            // Rich body (link preview card or image) — no extra URL label
            if entry.body.kind != .text {
                RichBodyView(entry: entry)
                    .onAppear { store.enrichEntryMetadata(entry) }
            }

            // Details / annotation (human-written, skips raw URLs)
            if let details = detailsText {
                let isURL = details.hasPrefix("http")
                if !isURL {
                    Text(details)
                        .font(.tnCaption)
                        .foregroundStyle(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
            }

            // Object mentions
            if !resource.mentionLabels.isEmpty {
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: TNSpacing.xs) {
                        ForEach(resource.mentionLabels, id: \.self) { label in
                            Text("@\(label)")
                                .font(.tnMicro.weight(.medium))
                                .padding(.horizontal, TNSpacing.sm)
                                .padding(.vertical, TNSpacing.xs)
                                .background(Color.tnBackground, in: Capsule())
                                .overlay(Capsule().stroke(Color.tnBorderSubtle, lineWidth: 1))
                        }
                    }
                }
            }

            // Actions
            HStack(spacing: TNSpacing.sm) {
                if let thread {
                    Button("Open Thread") { store.openThread(thread.id) }
                        .buttonStyle(.bordered)
                        .controlSize(.mini)
                }

                if entry.isSourceResource {
                    Button("View Source") { store.openSource(entry.id) }
                        .buttonStyle(.bordered)
                        .controlSize(.mini)
                }
            }
        }
        .padding(.vertical, TNSpacing.xs)
    }
}

private struct ResourceMetricPill: View {
    let kind: ResourceKind
    let count: Int

    var body: some View {
        HStack(spacing: TNSpacing.xs) {
            Image(systemName: kind.systemImage)
            Text(kind.title)
            Text("\(count)")
                .foregroundStyle(.secondary)
        }
        .font(.tnMicro.weight(.medium))
        .padding(.horizontal, TNSpacing.sm)
        .padding(.vertical, 7)
        .background(Color.tnSurface, in: Capsule())
        .overlay(
            Capsule()
                .stroke(Color.tnBorder, lineWidth: 1)
        )
    }
}

private struct ResourceInspectorLeadCard: View {
    let counts: ResourceCounts

    var body: some View {
        VStack(alignment: .leading, spacing: TNSpacing.md) {
            Text("Thread Distill")
                .font(.tnSectionTitle)

            Text("Keep links, media, and mentions nearby without turning the main thread into a browse-first page.")
                .font(.tnBody)
                .foregroundStyle(.secondary)

            ResourceOverviewRow(counts: counts)
        }
        .padding(TNSpacing.md)
        .background(Color.tnBackground.opacity(0.7), in: .rect(cornerRadius: TNCorner.lg))
        .overlay(
            RoundedRectangle(cornerRadius: TNCorner.lg)
                .stroke(Color.tnBorderSubtle, lineWidth: 1)
        )
    }
}

private extension ResourceKind {
    var panelDescription: String {
        switch self {
        case .link:
            "Reference material and sources that support the working stream."
        case .media:
            "Images, documents, and richer assets worth opening in context."
        case .mention:
            "Named objects the thread keeps circling back to."
        }
    }
}
