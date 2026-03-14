import Foundation

/// Incremental persistence boundary.
/// UI mutates in-memory state on the main actor, while disk writes are serialized off the main thread.
final class ThreadnoteRepository {
    private let store: PersistenceStore
    private let writeQueue = DispatchQueue(label: "ThreadnoteRepository.write", qos: .utility)

    let retrievalEngine: RetrievalEngine

    init(databaseURL: URL) throws {
        let store = try PersistenceStore(databaseURL: databaseURL)
        self.store = store
        self.retrievalEngine = RetrievalEngine(pool: store.databasePool)
    }

    func loadSnapshot() throws -> AppSnapshot {
        try store.loadSnapshot()
    }

    func saveSnapshot(_ snapshot: AppSnapshot) {
        schedule("saveSnapshot") { store in
            try store.saveSnapshot(snapshot)
        }
    }

    func fetchMemoryRecords(for threadID: UUID, scope: MemoryScope? = nil) -> [MemoryRecord] {
        (try? store.fetchMemoryRecords(for: threadID, scope: scope)) ?? []
    }

    func upsertThread(_ thread: ThreadRecord) {
        schedule("upsertThread") { store in
            try store.upsertThread(thread)
        }
    }

    func upsertEntry(_ entry: Entry) {
        schedule("upsertEntry") { store in
            try store.upsertEntry(entry)
        }
    }

    func deleteEntry(id: UUID) {
        schedule("deleteEntry") { store in
            try store.deleteEntry(id: id)
        }
    }

    func upsertClaim(_ claim: Claim) {
        schedule("upsertClaim") { store in
            try store.upsertClaim(claim)
        }
    }

    func deleteClaim(id: UUID) {
        schedule("deleteClaim") { store in
            try store.deleteClaim(id: id)
        }
    }

    func upsertAnchor(_ anchor: Anchor) {
        schedule("upsertAnchor") { store in
            try store.upsertAnchor(anchor)
        }
    }

    func upsertTask(_ task: ThreadTask) {
        schedule("upsertTask") { store in
            try store.upsertTask(task)
        }
    }

    func replaceDiscourseRelations(
        removingRelationsTouching entryIDs: Set<UUID>,
        with relations: [DiscourseRelation]
    ) {
        schedule("replaceDiscourseRelations") { store in
            try store.replaceDiscourseRelations(
                removingRelationsTouching: entryIDs,
                with: relations
            )
        }
    }

    func insertMemoryRecord(_ record: MemoryRecord) {
        schedule("insertMemoryRecord") { store in
            try store.insertMemoryRecord(record)
        }
    }

    private func schedule(
        _ operationName: String,
        _ operation: @Sendable @escaping (PersistenceStore) throws -> Void
    ) {
        let store = store
        writeQueue.async {
            do {
                try operation(store)
            } catch {
                print("[Repository] \(operationName) failed: \(error)")
            }
        }
    }
}
