import XCTest
@testable import ThreadnoteMVP

@MainActor
final class StoreURLBodyMigrationTests: XCTestCase {
    func testConfigureMigratesLegacyURLBodiesAndPersistsFlag() throws {
        let thread = TestFixtures.makeThread(title: "Legacy URL thread")
        let legacyEntry = Entry(
            id: UUID(),
            threadID: thread.id,
            kind: .note,
            body: EntryBody(kind: .text, text: "Look at https://example.com", url: nil, title: nil, details: nil, linkMeta: nil),
            summaryText: "Look at https://example.com",
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
        let url = try TestFixtures.seedDatabase(with: AppSnapshot(
            sampleDataVersion: 9,
            threads: [thread],
            entries: [legacyEntry],
            claims: [],
            anchors: [],
            tasks: [],
            discourseRelations: []
        ))

        let threadnoteStore = ThreadnoteStore(enableLLM: false)
        threadnoteStore.configure(with: url)

        let migratedEntry = try XCTUnwrap(threadnoteStore.entries.first)
        XCTAssertEqual(migratedEntry.body.kind, .mixed)
        XCTAssertEqual(migratedEntry.body.url, "https://example.com")
        XCTAssertEqual(migratedEntry.sourceMetadata?.locator, "https://example.com")

        let persistenceStore = try PersistenceStore(databaseURL: url)
        let persistedFlag = try persistenceStore.metadataValue(for: URLBodyMigration.metadataKey)
        let persistedEntry = try XCTUnwrap(persistenceStore.loadSnapshot().entries.first)
        XCTAssertEqual(persistedFlag, "1")
        XCTAssertEqual(persistedEntry.body.kind, .mixed)
        XCTAssertEqual(persistedEntry.body.url, "https://example.com")
    }

    func testConfigureMigratesLegacyAttachmentBodies() throws {
        let thread = TestFixtures.makeThread(title: "Legacy attachment thread")
        let legacyEntry = Entry(
            id: UUID(),
            threadID: thread.id,
            kind: .source,
            body: EntryBody(kind: .text, text: "attachments/preview.png", url: nil, title: nil, details: nil, linkMeta: nil),
            summaryText: "attachments/preview.png",
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
        let url = try TestFixtures.seedDatabase(with: AppSnapshot(
            sampleDataVersion: 9,
            threads: [thread],
            entries: [legacyEntry],
            claims: [],
            anchors: [],
            tasks: [],
            discourseRelations: []
        ))

        let store = ThreadnoteStore(enableLLM: false)
        store.configure(with: url)

        let migratedEntry = try XCTUnwrap(store.entries.first)
        XCTAssertEqual(migratedEntry.body.kind, .image)
        XCTAssertEqual(migratedEntry.body.url, "attachments/preview.png")
        XCTAssertEqual(migratedEntry.body.title, "preview.png")
        XCTAssertEqual(migratedEntry.sourceMetadata?.locator, "attachments/preview.png")
    }
}
