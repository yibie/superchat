import Foundation

@MainActor
final class ThreadnoteCompletionProvider: CompletionProvider {
    private let store: ThreadnoteStore

    init(store: ThreadnoteStore) {
        self.store = store
    }

    func completions(for trigger: CompletionTrigger) -> [CompletionItem] {
        switch trigger {
        case .tag(let query):
            return tagCompletions(query: query)
        case .object(let query):
            return objectCompletions(query: query)
        case .reference(let query):
            return referenceCompletions(query: query)
        }
    }

    // MARK: - Tag Completions

    private func tagCompletions(query: String) -> [CompletionItem] {
        let ranked = CaptureTag.allCases.compactMap { tag -> (CaptureTag, Int)? in
            let score = scoreTag(tag, query: query)
            guard score > 0 else { return nil }
            return (tag, score)
        }
        .sorted { lhs, rhs in
            if lhs.1 == rhs.1 {
                return lhs.0.suggestionRank < rhs.0.suggestionRank
            }
            return lhs.1 > rhs.1
        }
        .prefix(6)

        return ranked.map { tag, _ in
            CompletionItem(
                id: "tag-\(tag.rawValue)",
                title: tag.suggestionTitle,
                subtitle: tag.suggestionHint,
                icon: "number",
                insertionText: tag.insertionText,
                kind: .tag
            )
        }
    }

    private func scoreTag(_ tag: CaptureTag, query: String) -> Int {
        guard !query.isEmpty else {
            return max(1, 300 - tag.suggestionRank)
        }
        if tag.rawValue == query { return 1000 }
        if tag.rawValue.hasPrefix(query) { return 900 - query.count }
        if tag.suggestionSearchTokens.contains(where: { $0 == query }) { return 850 }
        if tag.suggestionSearchTokens.contains(where: { $0.hasPrefix(query) }) { return 760 - query.count }
        if tag.suggestionTitle.lowercased().contains(query) { return 640 - query.count }
        if tag.suggestionHint.lowercased().contains(query) { return 520 - query.count }
        if tag.suggestionSearchTokens.contains(where: { $0.contains(query) }) { return 480 - query.count }
        return 0
    }

    // MARK: - Object Completions

    private func objectCompletions(query: String) -> [CompletionItem] {
        let names = store.uniqueObjectNames
        let filtered: [String]
        if query.isEmpty {
            filtered = Array(names.prefix(6))
        } else {
            filtered = names.filter { $0.lowercased().contains(query) }.prefix(6).map { $0 }
        }

        return filtered.map { name in
            CompletionItem(
                id: "object-\(name)",
                title: name,
                subtitle: "Object mention",
                icon: "at",
                insertionText: name,
                kind: .object
            )
        }
    }

    // MARK: - Reference Completions

    private func referenceCompletions(query: String) -> [CompletionItem] {
        let candidates = store.referenceCompletionCandidates
        let filtered: [(String, String, String)]
        if query.isEmpty {
            filtered = Array(candidates.prefix(6))
        } else {
            filtered = candidates.filter { $0.0.lowercased().contains(query) }.prefix(6).map { $0 }
        }

        return filtered.map { title, subtitle, icon in
            CompletionItem(
                id: "ref-\(title)",
                title: title,
                subtitle: subtitle,
                icon: icon,
                insertionText: title,
                kind: .reference
            )
        }
    }
}
