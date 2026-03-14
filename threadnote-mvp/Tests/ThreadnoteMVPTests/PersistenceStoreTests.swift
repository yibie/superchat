import XCTest
@testable import ThreadnoteMVP

final class PersistenceStoreTests: XCTestCase {
    func testSaveSnapshotReplacesRemovedRows() throws {
        let store = try makeStore()
        let thread = makeThread(title: "Thread A")
        let firstEntry = makeEntry(threadID: thread.id, text: "first")
        let secondEntry = makeEntry(threadID: thread.id, text: "second")

        try store.saveSnapshot(AppSnapshot(
            sampleDataVersion: 9,
            threads: [thread],
            entries: [firstEntry],
            claims: [],
            anchors: [],
            tasks: [],
            discourseRelations: []
        ))

        try store.saveSnapshot(AppSnapshot(
            sampleDataVersion: 9,
            threads: [thread],
            entries: [secondEntry],
            claims: [],
            anchors: [],
            tasks: [],
            discourseRelations: []
        ))

        let loaded = try store.loadSnapshot()
        XCTAssertEqual(loaded.entries.map(\.id), [secondEntry.id])
        XCTAssertEqual(loaded.threads.map(\.id), [thread.id])
    }

    func testReplaceDiscourseRelationsRemovesTouchedRows() throws {
        let store = try makeStore()
        let thread = makeThread(title: "Thread B")
        let entryA = makeEntry(threadID: thread.id, text: "A")
        let entryB = makeEntry(threadID: thread.id, text: "B")
        let relation = DiscourseRelation(
            id: UUID(),
            sourceEntryID: entryA.id,
            targetEntryID: entryB.id,
            kind: .informs,
            confidence: 0.7
        )

        try store.saveSnapshot(AppSnapshot(
            sampleDataVersion: 9,
            threads: [thread],
            entries: [entryA, entryB],
            claims: [],
            anchors: [],
            tasks: [],
            discourseRelations: [relation]
        ))

        try store.replaceDiscourseRelations(removingRelationsTouching: [entryA.id, entryB.id], with: [])

        let loaded = try store.loadSnapshot()
        XCTAssertTrue(loaded.discourseRelations.isEmpty)
    }

    private func makeStore() throws -> PersistenceStore {
        let tempURL = FileManager.default.temporaryDirectory
            .appendingPathComponent(UUID().uuidString)
            .appendingPathExtension("sqlite")
        return try PersistenceStore(databaseURL: tempURL)
    }

    private func makeThread(title: String) -> ThreadRecord {
        ThreadRecord(
            id: UUID(),
            title: title,
            prompt: title,
            goalLayer: ThreadGoalLayer(
                goalStatement: title,
                goalType: .research,
                successCondition: "Done",
                currentStage: .framing
            ),
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now,
            color: .sky
        )
    }

    private func makeEntry(threadID: UUID, text: String) -> Entry {
        Entry(
            id: UUID(),
            threadID: threadID,
            kind: .note,
            body: EntryBody(kind: .text, text: text, url: nil, title: nil, details: nil),
            summaryText: text,
            sourceMetadata: nil,
            createdAt: .now,
            sessionID: nil,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: nil,
            confidenceScore: nil,
            inboxState: "resolved"
        )
    }
}
