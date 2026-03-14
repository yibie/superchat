import SwiftUI
import XCTest
@testable import ThreadnoteMVP

final class SharedComponentsTests: XCTestCase {
    func testEntryBodyTextHighlightsMention() throws {
        let attributed = entryBodyText(text: "Talk to @Alice tomorrow.", font: .tnBody)
        let text = String(attributed.characters)
        let mentionRange = try XCTUnwrap(text.range(of: "@Alice"))
        let attributedRange = try XCTUnwrap(Range(mentionRange, in: attributed))

        XCTAssertNotNil(attributed[attributedRange].foregroundColor)
        XCTAssertNotNil(attributed[attributedRange].font)
    }

    func testEntryBodyTextHighlightsReference() throws {
        let attributed = entryBodyText(text: "Review [[Atlas launch thread]] next.", font: .tnBody)
        let text = String(attributed.characters)
        let referenceRange = try XCTUnwrap(text.range(of: "[[Atlas launch thread]]"))
        let attributedRange = try XCTUnwrap(Range(referenceRange, in: attributed))

        XCTAssertNotNil(attributed[attributedRange].foregroundColor)
        XCTAssertNotNil(attributed[attributedRange].font)
        XCTAssertNotNil(attributed[attributedRange].underlineStyle)
    }
}
