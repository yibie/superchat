import XCTest
@testable import ThreadnoteMVP

final class URLBodyMigrationTests: XCTestCase {
    func testRunIfNeededBackfillsMixedURLBodyAndPreservesExistingMetadata() {
        let existingMetadata = SourceMetadata(title: "Saved title", locator: nil, citation: "Saved citation")
        var entries = [
            Entry(
                id: UUID(),
                threadID: UUID(),
                kind: .note,
                body: EntryBody(kind: .text, text: "Read https://example.com", url: nil, title: nil, details: nil, linkMeta: nil),
                summaryText: "Read https://example.com",
                sourceMetadata: existingMetadata,
                createdAt: .now,
                sessionID: nil,
                authorType: "user",
                parentEntryID: nil,
                supersedesEntryID: nil,
                importanceScore: nil,
                confidenceScore: nil,
                inboxState: "resolved"
            )
        ]

        let modified = URLBodyMigration.runIfNeeded(entries: &entries)

        XCTAssertEqual(modified.count, 1)
        XCTAssertEqual(entries[0].body.kind, .mixed)
        XCTAssertEqual(entries[0].body.text, "Read https://example.com")
        XCTAssertEqual(entries[0].body.url, "https://example.com")
        XCTAssertEqual(entries[0].sourceMetadata?.title, "Saved title")
        XCTAssertEqual(entries[0].sourceMetadata?.citation, "Saved citation")
        XCTAssertEqual(entries[0].sourceMetadata?.locator, "https://example.com")
    }

    func testRunIfNeededBackfillsPureURLBody() {
        var entries = [
            Entry(
                id: UUID(),
                threadID: UUID(),
                kind: .source,
                body: EntryBody(kind: .text, text: "https://example.com/path", url: nil, title: nil, details: nil, linkMeta: nil),
                summaryText: " https://example.com/path \n",
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
        ]

        let modified = URLBodyMigration.runIfNeeded(entries: &entries)

        XCTAssertEqual(modified.count, 1)
        XCTAssertEqual(entries[0].body.kind, .url)
        XCTAssertNil(entries[0].body.text)
        XCTAssertEqual(entries[0].body.url, "https://example.com/path")
        XCTAssertEqual(entries[0].sourceMetadata?.locator, "https://example.com/path")
    }

    func testAttachmentBodyIfNeededClassifiesPureImagePath() {
        let body = URLBodyMigration.attachmentBodyIfNeeded(from: "attachments/3f9c2a.png")

        XCTAssertEqual(body?.kind, .image)
        XCTAssertEqual(body?.url, "attachments/3f9c2a.png")
        XCTAssertEqual(body?.title, "3f9c2a.png")
        XCTAssertEqual(body?.linkMeta?.contentType, .image)
        XCTAssertNil(body?.text)
    }

    func testAttachmentBodyIfNeededClassifiesVideoAndAudioPaths() {
        let videoBody = URLBodyMigration.attachmentBodyIfNeeded(from: "attachments/demo.mp4")
        let audioBody = URLBodyMigration.attachmentBodyIfNeeded(from: "attachments/voice.m4a")

        XCTAssertEqual(videoBody?.kind, .url)
        XCTAssertEqual(videoBody?.linkMeta?.contentType, .video)
        XCTAssertEqual(audioBody?.kind, .url)
        XCTAssertEqual(audioBody?.linkMeta?.contentType, .audio)
    }

    func testAttachmentBodyIfNeededBuildsMixedBodyWhenAttachmentSharesLineWithText() {
        let body = URLBodyMigration.attachmentBodyIfNeeded(
            from: "Need to review attachments/spec-sheet.pdf before tomorrow."
        )

        XCTAssertEqual(body?.kind, .mixed)
        XCTAssertEqual(body?.url, "attachments/spec-sheet.pdf")
        XCTAssertEqual(body?.text, "Need to review before tomorrow.")
        XCTAssertEqual(body?.title, "spec-sheet.pdf")
        XCTAssertEqual(body?.linkMeta?.contentType, .document)
    }
}
