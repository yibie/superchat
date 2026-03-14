import XCTest
@testable import ThreadnoteMVP

final class CaptureInterpreterTests: XCTestCase {
    private let interpreter = CaptureInterpreter()

    func testExplicitTagOverridesInferenceAndNormalizesText() {
        let interpretation = interpreter.interpret(text: "#plan Follow up with OpenAI about Atlas rollout")

        XCTAssertEqual(interpretation.explicitTag, .plan)
        XCTAssertEqual(interpretation.detectedItemType, .plan)
        XCTAssertEqual(interpretation.normalizedText, "Follow up with OpenAI about Atlas rollout")
    }

    func testInfersQuestionAndExtractsNaturalLanguageObjects() {
        let interpretation = interpreter.interpret(text: "Why is OpenAI delaying Atlas launch in San Francisco?")

        XCTAssertEqual(interpretation.detectedItemType, .question)
        XCTAssertTrue(interpretation.detectedObjects.contains(where: { $0.name == "OpenAI" }))
        XCTAssertTrue(interpretation.detectedObjects.contains(where: { $0.name == "Atlas" }))
        XCTAssertTrue(interpretation.detectedObjects.contains(where: { $0.name == "San Francisco" }))
    }

    func testExtractsCandidateClaimFromPlainDeclarativeNote() {
        let interpretation = interpreter.interpret(
            text: "OpenAI Atlas pricing is too complex for the current launch."
        )

        XCTAssertEqual(interpretation.detectedItemType, .claim)
        XCTAssertEqual(interpretation.candidateClaims.first?.text, "OpenAI Atlas pricing is too complex for the current launch")
        XCTAssertGreaterThan(interpretation.confidenceScore, 0.6)
    }
}
