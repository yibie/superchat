import XCTest
@testable import ThreadnoteMVP

final class ResourceDerivationTests: XCTestCase {
    func testPlainURLNoteClassifiesAsLink() {
        let entry = makeEntry(
            body: EntryBody(
                kind: .url,
                text: nil,
                url: "https://example.com/article",
                title: nil,
                details: nil,
                linkMeta: nil
            )
        )

        XCTAssertEqual(ResourceDerivation.classify(entry), .link)
    }

    func testMediaMetadataClassifiesAsMedia() {
        let entry = makeEntry(
            body: EntryBody(
                kind: .url,
                text: nil,
                url: "https://example.com/video",
                title: nil,
                details: nil,
                linkMeta: LinkMetadata(
                    metaTitle: "Demo",
                    metaDescription: nil,
                    imageURL: nil,
                    siteName: nil,
                    videoID: "abc123",
                    contentType: .video
                )
            )
        )

        XCTAssertEqual(ResourceDerivation.classify(entry), .media)
    }

    func testMentionOnlyNoteClassifiesAsMention() {
        let entry = makeEntry(
            body: EntryBody(kind: .text, text: "Talk to @OpenAI", url: nil, title: nil, details: nil, linkMeta: nil),
            objectMentions: [
                ObjectMention(id: UUID(), name: "OpenAI", kind: .company)
            ]
        )

        XCTAssertEqual(ResourceDerivation.classify(entry), .mention)
    }

    func testURLAndMentionNoteUsesSingleHighestPriorityType() {
        let entry = makeEntry(
            body: EntryBody(
                kind: .url,
                text: nil,
                url: "https://example.com/report",
                title: nil,
                details: nil,
                linkMeta: nil
            ),
            objectMentions: [
                ObjectMention(id: UUID(), name: "Alpha", kind: .company),
                ObjectMention(id: UUID(), name: "Alpha", kind: .company),
                ObjectMention(id: UUID(), name: "Beta", kind: .product)
            ]
        )

        let resources = ResourceDerivation.derive(from: [entry])
        XCTAssertEqual(resources.count, 1)
        XCTAssertEqual(resources.first?.kind, .link)
        XCTAssertEqual(resources.first?.mentionLabels, ["Alpha", "Beta"])
    }

    func testMetadataUpgradeCanPromoteLinkToMedia() {
        let baseBody = EntryBody(
            kind: .url,
            text: nil,
            url: "https://example.com/asset",
            title: nil,
            details: nil,
            linkMeta: nil
        )
        let linkEntry = makeEntry(body: baseBody)

        var mediaBody = baseBody
        mediaBody.linkMeta = LinkMetadata(
            metaTitle: "Asset",
            metaDescription: nil,
            imageURL: "https://example.com/asset.jpg",
            siteName: nil,
            videoID: nil,
            contentType: .image
        )
        let mediaEntry = makeEntry(id: linkEntry.id, body: mediaBody)

        XCTAssertEqual(ResourceDerivation.classify(linkEntry), .link)
        XCTAssertEqual(ResourceDerivation.classify(mediaEntry), .media)
        XCTAssertEqual(ResourceDerivation.derive(from: [mediaEntry]).count, 1)
    }

    func testSidebarStateOpensSelectsAndResetsTabs() {
        var state = ThreadSidebarState()

        XCTAssertFalse(state.isPresented)
        XCTAssertEqual(state.selectedTab.id, ThreadSidebarTab.restartNote.id)

        state.open(role: .restartNote)
        XCTAssertTrue(state.isPresented)
        XCTAssertEqual(state.selectedTab.id, ThreadSidebarTab.restartNote.id)

        state.close()
        XCTAssertFalse(state.isPresented)
        XCTAssertEqual(state.selectedTab.id, ThreadSidebarTab.restartNote.id)

        state.select(tabID: ThreadSidebarTab.resources.id)
        XCTAssertTrue(state.isPresented)
        XCTAssertEqual(state.selectedTab.id, ThreadSidebarTab.resources.id)

        state.reset()
        XCTAssertFalse(state.isPresented)
        XCTAssertEqual(state.selectedTab.id, ThreadSidebarTab.restartNote.id)
        XCTAssertEqual(state.fixedTabs.count, 2)
        XCTAssertTrue(state.extraTabs.isEmpty)
    }

    private func makeEntry(
        id: UUID = UUID(),
        threadID: UUID = UUID(),
        kind: EntryKind = .note,
        body: EntryBody,
        summaryText: String = "Summary",
        objectMentions: [ObjectMention] = []
    ) -> Entry {
        Entry(
            id: id,
            threadID: threadID,
            kind: kind,
            body: body,
            summaryText: summaryText,
            sourceMetadata: nil,
            objectMentions: objectMentions,
            references: [],
            createdAt: Date(timeIntervalSince1970: 1_000),
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
