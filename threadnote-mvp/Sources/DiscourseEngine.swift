import Foundation

func primaryRelation(for entryID: UUID, relations: [DiscourseRelation]) -> DiscourseRelation? {
    relations.first(where: { $0.sourceEntryID == entryID })
        ?? relations.first(where: { $0.targetEntryID == entryID })
}

func relationTarget(for entry: Entry, previousEntries: [Entry]) -> Entry? {
    switch entry.kind {
    case .evidence:
        return previousEntries.first(where: { $0.kind == .claim })
            ?? previousEntries.first(where: { $0.kind == .source })
            ?? previousEntries.first(where: { $0.kind == .evidence })
    case .source:
        return nil
    case .question:
        return nil
    case .claim, .comparison, .pattern, .decided, .verified:
        return previousEntries.first(where: { $0.kind == .question })
            ?? previousEntries.first(where: { $0.kind == .claim || $0.kind == .evidence })
    case .note, .idea, .plan, .solved, .dropped, .handoff, .anchorWritten:
        return nil
    }
}

func relationKind(for entry: Entry, target: Entry) -> DiscourseRelationKind {
    switch entry.kind {
    case .evidence:
        if target.kind == .source {
            return .informs
        }
        return isOpposingNarrative(entry.summaryText) ? .opposes : .supports
    case .source:
        return .informs
    case .question:
        return .answers
    case .claim, .comparison, .pattern, .decided, .verified:
        if target.kind == .question {
            return .answers
        }
        return isOpposingNarrative(entry.summaryText) ? .opposes : .supports
    case .note, .idea, .plan, .solved, .dropped, .handoff, .anchorWritten:
        return .supports
    }
}

func isOpposingNarrative(_ text: String) -> Bool {
    let lowered = text.lowercased()
    return lowered.contains("not")
        || lowered.contains("instead")
        || lowered.contains("however")
        || lowered.contains("fails")
        || lowered.contains("doesn't")
        || lowered.contains("does not")
}
