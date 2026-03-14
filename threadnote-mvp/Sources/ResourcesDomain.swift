import Foundation

enum ResourceKind: String, CaseIterable, Identifiable {
    case link
    case media
    case mention

    var id: Self { self }

    var title: String {
        switch self {
        case .link:
            "Links"
        case .media:
            "Media"
        case .mention:
            "Mentions"
        }
    }

    var singularTitle: String {
        switch self {
        case .link:
            "Link"
        case .media:
            "Media"
        case .mention:
            "Mention"
        }
    }

    var systemImage: String {
        switch self {
        case .link:
            "link"
        case .media:
            "photo.on.rectangle"
        case .mention:
            "at"
        }
    }
}

struct ResourceItem: Identifiable, Hashable {
    let entry: Entry
    let threadID: UUID
    let kind: ResourceKind
    let mentionLabels: [String]

    var id: UUID { entry.id }
    var createdAt: Date { entry.createdAt }

    var title: String {
        switch kind {
        case .mention:
            return entry.summaryText
        case .link, .media:
            return entry.sourceDisplayTitle
        }
    }

    var previewText: String {
        if let locator = entry.sourceLocator, !locator.isEmpty, locator != title {
            return locator
        }
        return entry.summaryText
    }
}

struct ResourceCounts: Hashable {
    var linkCount = 0
    var mediaCount = 0
    var mentionCount = 0

    var totalCount: Int {
        linkCount + mediaCount + mentionCount
    }

    subscript(kind: ResourceKind) -> Int {
        get {
            switch kind {
            case .link:
                return linkCount
            case .media:
                return mediaCount
            case .mention:
                return mentionCount
            }
        }
        set {
            switch kind {
            case .link:
                linkCount = newValue
            case .media:
                mediaCount = newValue
            case .mention:
                mentionCount = newValue
            }
        }
    }
}

enum ResourceDerivation {
    static func classify(_ entry: Entry) -> ResourceKind? {
        if isMedia(entry) {
            return .media
        }
        if isLink(entry) {
            return .link
        }
        if isMention(entry) {
            return .mention
        }
        return nil
    }

    static func derive(from entries: [Entry]) -> [ResourceItem] {
        entries
            .compactMap { entry in
                guard let threadID = entry.threadID,
                      let kind = classify(entry) else { return nil }
                return ResourceItem(
                    entry: entry,
                    threadID: threadID,
                    kind: kind,
                    mentionLabels: mentionLabels(for: entry)
                )
            }
            .sorted { $0.createdAt > $1.createdAt }
    }

    static func counts(from resources: [ResourceItem]) -> ResourceCounts {
        resources.reduce(into: ResourceCounts()) { counts, resource in
            counts[resource.kind] += 1
        }
    }

    static func mentionLabels(for entry: Entry) -> [String] {
        Array(Set(entry.objectMentions.map(\.name)))
            .sorted { $0.localizedCaseInsensitiveCompare($1) == .orderedAscending }
    }

    private static func isMedia(_ entry: Entry) -> Bool {
        switch entry.body.kind {
        case .image, .document:
            return true
        case .text, .url, .mixed:
            break
        }

        switch entry.body.linkMeta?.contentType {
        case .image, .video, .audio, .document:
            return true
        case .webpage, .none:
            return false
        }
    }

    private static func isLink(_ entry: Entry) -> Bool {
        entry.isAttachmentResource
    }

    private static func isMention(_ entry: Entry) -> Bool {
        entry.isObjectResourceNote
    }
}

enum ThreadSidebarTabRole: String, Hashable {
    case restartNote
    case resources
    case extra
}

struct ThreadSidebarTab: Identifiable, Hashable {
    let id: String
    let title: String
    let systemImage: String
    let role: ThreadSidebarTabRole

    static let restartNote = ThreadSidebarTab(
        id: "restartNote",
        title: "Restart Note",
        systemImage: "arrow.clockwise.circle",
        role: .restartNote
    )

    static let resources = ThreadSidebarTab(
        id: "resources",
        title: "Resources",
        systemImage: "books.vertical",
        role: .resources
    )

    static func fixedTabs() -> [ThreadSidebarTab] {
        [.restartNote, .resources]
    }
}

struct ThreadSidebarState: Hashable {
    var isPresented = false
    var selectedTabID = ThreadSidebarTab.restartNote.id
    var fixedTabs = ThreadSidebarTab.fixedTabs()
    var extraTabs: [ThreadSidebarTab] = []
    var contextThreadID: UUID?

    var allTabs: [ThreadSidebarTab] {
        fixedTabs + extraTabs
    }

    var selectedTab: ThreadSidebarTab {
        allTabs.first(where: { $0.id == selectedTabID }) ?? ThreadSidebarTab.restartNote
    }

    mutating func open(role: ThreadSidebarTabRole) {
        guard let tab = allTabs.first(where: { $0.role == role }) else { return }
        isPresented = true
        selectedTabID = tab.id
    }

    mutating func select(tabID: String) {
        guard allTabs.contains(where: { $0.id == tabID }) else { return }
        isPresented = true
        selectedTabID = tabID
    }

    mutating func close() {
        isPresented = false
    }

    mutating func reset() {
        self = ThreadSidebarState()
    }
}
