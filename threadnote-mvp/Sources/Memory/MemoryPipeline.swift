/// task015–018 — MemoryPipeline.
/// Sync writes for 4 memory scopes. Each write path is a single targeted insert;
/// no LLM or async work in this layer (that lives in M4).

import Foundation

@MainActor
final class MemoryPipeline {

    private let repository: ThreadnoteRepository

    init(repository: ThreadnoteRepository) {
        self.repository = repository
    }

    // MARK: - Working memory (task015)
    // Called on every entry append when entry belongs to a thread.

    func recordWorking(entry: Entry) {
        guard let threadID = entry.threadID else { return }
        // Only record substantive user entries
        let substantiveKinds: Set<EntryKind> = [
            .note, .claim, .question, .evidence, .source,
            .idea, .plan, .decided, .solved, .verified, .dropped
        ]
        guard substantiveKinds.contains(entry.kind) else { return }
        let text = entry.summaryText.isEmpty ? (entry.body.text ?? "") : entry.summaryText
        guard !text.isEmpty else { return }

        let record = MemoryRecord(
            id: UUID(),
            threadID: threadID,
            scope: .working,
            text: text,
            provenance: "entry:\(entry.id.uuidString)",
            createdAt: entry.createdAt
        )
        persist(record)
    }

    // MARK: - Episodic memory (task016)
    // Called when an Anchor is written.

    func recordEpisodic(anchor: Anchor) {
        let parts = [anchor.stateSummary, anchor.coreQuestion]
            .filter { !$0.isEmpty }
        let text = parts.joined(separator: " — ")
        guard !text.isEmpty else { return }

        let record = MemoryRecord(
            id: UUID(),
            threadID: anchor.threadID,
            scope: .episodic,
            text: text,
            provenance: "anchor:\(anchor.id.uuidString)",
            createdAt: anchor.createdAt
        )
        persist(record)
    }

    // MARK: - Semantic memory (task017)
    // Called when a claim reaches a settled status.

    func recordSemantic(claim: Claim) {
        let settledStatuses: Set<ClaimStatus> = [.stable]
        guard settledStatuses.contains(claim.status) else { return }

        let text = claim.statement
        guard !text.isEmpty else { return }

        let record = MemoryRecord(
            id: UUID(),
            threadID: claim.threadID,
            scope: .semantic,
            text: text,
            provenance: "claim:\(claim.id.uuidString)",
            createdAt: claim.updatedAt
        )
        // Replace any existing semantic record for this claim to avoid duplicates
        let existing = repository.fetchMemoryRecords(for: claim.threadID, scope: .semantic)
        if existing.contains(where: { $0.provenance == "claim:\(claim.id.uuidString)" }) {
            return
        }
        persist(record)
    }

    // MARK: - Source memory (task018)
    // Called when a source-kind entry is appended.

    func recordSource(entry: Entry) {
        guard let threadID = entry.threadID else { return }
        guard entry.kind == .source else { return }

        let title = entry.body.title ?? entry.summaryText
        let url = entry.body.url ?? ""
        let text = [title, url].filter { !$0.isEmpty }.joined(separator: " ")
        guard !text.isEmpty else { return }

        let record = MemoryRecord(
            id: UUID(),
            threadID: threadID,
            scope: .source,
            text: text,
            provenance: "entry:\(entry.id.uuidString)",
            createdAt: entry.createdAt
        )
        persist(record)
    }

    // MARK: - Private

    private func persist(_ record: MemoryRecord) {
        repository.insertMemoryRecord(record)
    }
}
