import Foundation

func parseObjectMentions(in text: String) -> [ObjectMention] {
    guard let regex = try? NSRegularExpression(pattern: #"(?<!\S)@([\p{L}\p{N}][\p{L}\p{N}._-]*)"#, options: []) else {
        return []
    }
    let range = NSRange(text.startIndex..<text.endIndex, in: text)
    let names = regex.matches(in: text, options: [], range: range)
        .compactMap { match -> String? in
            guard let nameRange = Range(match.range(at: 1), in: text) else { return nil }
            return String(text[nameRange])
        }

    var seen = Set<String>()
    return names.compactMap { name in
        let key = name.lowercased()
        guard seen.insert(key).inserted else { return nil }
        return ObjectMention(id: UUID(), name: name, kind: inferObjectKind(for: name))
    }
}

func inferObjectKind(for name: String) -> ObjectKind {
    let lowered = name.lowercased()
    if lowered.hasSuffix("inc") || lowered.hasSuffix("labs") || lowered.hasSuffix("corp") || lowered.contains("openclaw") {
        return .company
    }
    return .generic
}
