import Foundation

enum CompletionTrigger {
    case tag(query: String)
    case object(query: String)
    case reference(query: String)
}

enum CompletionItemKind {
    case tag
    case object
    case reference
}

struct CompletionItem: Identifiable, Hashable {
    let id: String
    let title: String
    let subtitle: String
    let icon: String
    let insertionText: String
    let kind: CompletionItemKind
}

@MainActor
protocol CompletionProvider {
    func completions(for trigger: CompletionTrigger) -> [CompletionItem]
}
