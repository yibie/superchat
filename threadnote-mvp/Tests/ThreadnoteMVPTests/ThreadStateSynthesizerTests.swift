import XCTest
@testable import ThreadnoteMVP

final class ThreadStateSynthesizerTests: XCTestCase {
    private let helper = DeterministicAIHelper()

    func testSynthesizerBuildsThreadSpecificStateFromClaimAndEvidence() {
        let thread = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let claim = TestFixtures.makeClaim(
            threadID: thread.id,
            statement: "Atlas pricing should stay usage-based",
            status: .stable
        )
        let evidence = TestFixtures.makeEntry(
            threadID: thread.id,
            kind: .evidence,
            text: "Usage-based pricing matches how Atlas customers evaluate spend."
        )
        let signature = ThreadSignature(
            thread: thread,
            goalStatement: thread.goalLayer.goalStatement,
            coreObjects: [],
            activeClaims: [claim.statement],
            latestAnchorSummary: nil,
            openLoops: [],
            lastActiveAt: thread.lastActiveAt
        )
        let input = ThreadStateInput(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            signature: signature,
            activeClaims: [claim],
            questions: [],
            evidenceEntries: [evidence],
            sourceEntries: [],
            recentEntries: [evidence],
            recentNotes: [AISnippet(id: evidence.id, text: evidence.summaryText, kind: evidence.kind)],
            anchor: nil,
            entriesSinceAnchor: [evidence],
            discourseRelations: []
        )

        let snapshot = helper.synthesizeThreadState(input: input)

        XCTAssertEqual(snapshot.currentJudgment, claim.statement)
        XCTAssertFalse(snapshot.judgmentBasis.isEmpty)
        XCTAssertFalse(snapshot.presentation.headline.isEmpty)
        XCTAssertEqual(snapshot.presentation.blocks.first?.kind, .judgment)
        XCTAssertEqual(
            snapshot.recoveryLines.map(\.title),
            ["Current Judgment", "Why This Holds", "Main Gap", "Next Move"]
        )
        XCTAssertEqual(
            snapshot.openLoops.first,
            "The strongest evidence is still ungrounded by a source or reference."
        )
        XCTAssertTrue(snapshot.nextAction?.contains("Link a source") ?? false)
        XCTAssertEqual(snapshot.resolvedSoFar.first?.statusLabel, "Stable claim")
    }

    func testResumeEnrichmentPreservesDeterministicCoreState() {
        let snapshot = ThreadStateSnapshot(
            currentJudgment: "Atlas pricing should stay usage-based",
            judgmentBasis: "Two evidence notes support it.",
            openLoops: ["Ground the strongest evidence with a source."],
            nextAction: "Link a source that grounds the strongest evidence.",
            presentation: ThreadPresentation(
                headline: "You have a working read, but it still needs evidence before you can trust it.",
                blocks: [
                    ThreadBlock(
                        kind: .judgment,
                        title: "Working Read",
                        summary: "Atlas pricing should stay usage-based",
                        items: ["Two evidence notes support it."],
                        tone: .accent,
                        provenanceEntryIDs: []
                    )
                ],
                primaryAction: "Link a source that grounds the strongest evidence."
            ),
            restartNote: "Thread focus: Atlas pricing. Current judgment: usage-based pricing.",
            recoveryLines: [
                ResumeRecoveryLine(title: "Current Judgment", body: "Atlas pricing should stay usage-based"),
                ResumeRecoveryLine(title: "Why This Holds", body: "Two evidence notes support it."),
                ResumeRecoveryLine(title: "Main Gap", body: "Ground the strongest evidence with a source."),
                ResumeRecoveryLine(title: "Next Move", body: "Link a source that grounds the strongest evidence.")
            ],
            resolvedSoFar: [
                ResolvedItem(text: "Atlas pricing should stay usage-based", statusLabel: "Stable claim", resolvedAt: .now)
            ]
        )
        let llmResult = ResumeSynthesisResult(
            currentJudgment: "Switch Atlas pricing immediately",
            openLoops: ["Rewrite the whole pricing model."],
            nextAction: "Drop the current claim.",
            restartNote: "You are still working on Atlas pricing. Keep the usage-based model and ground it with a source.",
            recoveryLines: [
                ResumeRecoveryLine(title: "Wrong Title", body: "Usage-based pricing still fits the current evidence."),
                ResumeRecoveryLine(title: "Wrong Title", body: "The thread already has two supporting evidence notes."),
                ResumeRecoveryLine(title: "Wrong Title", body: "A source is still missing for the strongest evidence."),
                ResumeRecoveryLine(title: "Wrong Title", body: "Link one source before broadening the thread.")
            ],
            resolvedSoFar: [
                ResolvedItem(text: "Wrong state", statusLabel: "Wrong", resolvedAt: .now)
            ]
        )

        let enriched = helper.enrichedResume(snapshot: snapshot, llmResult: llmResult)

        XCTAssertEqual(enriched.currentJudgment, snapshot.currentJudgment)
        XCTAssertEqual(enriched.openLoops, snapshot.openLoops)
        XCTAssertEqual(enriched.nextAction, snapshot.nextAction)
        XCTAssertEqual(enriched.resolvedSoFar, snapshot.resolvedSoFar)
        XCTAssertEqual(enriched.restartNote, llmResult.restartNote)
        XCTAssertEqual(enriched.recoveryLines.map(\.title), snapshot.recoveryLines.map(\.title))
        XCTAssertEqual(enriched.recoveryLines.last?.body, "Link one source before broadening the thread.")
    }

    func testPresentationGroupsStateIntoBlocksInsteadOfDuplicatedProse() {
        let thread = TestFixtures.makeThread(title: "First thread")
        let latest = TestFixtures.makeEntry(threadID: thread.id, text: "My first thread note.")
        let signature = ThreadSignature(
            thread: thread,
            goalStatement: thread.goalLayer.goalStatement,
            coreObjects: [],
            activeClaims: [],
            latestAnchorSummary: nil,
            openLoops: [],
            lastActiveAt: thread.lastActiveAt
        )
        let input = ThreadStateInput(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            signature: signature,
            activeClaims: [],
            questions: [],
            evidenceEntries: [],
            sourceEntries: [],
            recentEntries: [latest],
            recentNotes: [AISnippet(id: latest.id, text: latest.summaryText, kind: latest.kind)],
            anchor: nil,
            entriesSinceAnchor: [latest],
            discourseRelations: []
        )

        let snapshot = helper.synthesizeThreadState(input: input)

        XCTAssertFalse(snapshot.presentation.headline.isEmpty)
        XCTAssertEqual(snapshot.presentation.blocks.map(\.kind), [.judgment, .gap, .nextMove, .resolved])
        XCTAssertEqual(snapshot.presentation.blocks.first?.summary, "My first thread note.")
        XCTAssertEqual(snapshot.presentation.blocks[1].title, "Main Gap")
        XCTAssertEqual(snapshot.presentation.primaryAction, snapshot.nextAction)
    }

    func testAIPresentationPlanCanChangeBlockComposition() {
        let thread = TestFixtures.makeThread(title: "Pensieve design principles")
        let claim = TestFixtures.makeClaim(
            threadID: thread.id,
            statement: "A Pensieve-like tool should externalize memory without pretending to replace judgment.",
            status: .working
        )
        let evidence = TestFixtures.makeEntry(
            threadID: thread.id,
            kind: .evidence,
            text: "Pensieve lets someone revisit memory, but it also risks overwhelming the user."
        )
        let question = TestFixtures.makeEntry(
            threadID: thread.id,
            kind: .question,
            text: "Which parts of the Pensieve metaphor map to UI, and which are only lore?"
        )
        let signature = ThreadSignature(
            thread: thread,
            goalStatement: thread.goalLayer.goalStatement,
            coreObjects: [],
            activeClaims: [claim.statement],
            latestAnchorSummary: nil,
            openLoops: [],
            lastActiveAt: thread.lastActiveAt
        )
        let input = ThreadStateInput(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            signature: signature,
            activeClaims: [claim],
            questions: [question],
            evidenceEntries: [evidence],
            sourceEntries: [],
            recentEntries: [question, evidence],
            recentNotes: [
                AISnippet(id: evidence.id, text: evidence.summaryText, kind: evidence.kind),
                AISnippet(id: question.id, text: question.summaryText, kind: question.kind)
            ],
            anchor: nil,
            entriesSinceAnchor: [question, evidence],
            discourseRelations: []
        )
        let snapshot = helper.synthesizeThreadState(input: input)
        let llmResult = ResumeSynthesisResult(
            currentJudgment: snapshot.currentJudgment,
            openLoops: snapshot.openLoops,
            nextAction: snapshot.nextAction,
            restartNote: "You are designing around one core tension: memory replay should help judgment, not replace it.",
            recoveryLines: snapshot.recoveryLines,
            resolvedSoFar: snapshot.resolvedSoFar,
            presentationPlan: ThreadPresentationPlan(
                headline: "Focus the cockpit on the core design tension, not on raw note categories.",
                blocks: [
                    ThreadBlockPlan(
                        kind: .principles,
                        title: "Design Principles",
                        summary: "Externalize memory, but keep judgment visibly separate.",
                        items: [
                            "Replay should support recall, not think for the user.",
                            "The interface needs a clear boundary between memory and interpretation."
                        ],
                        tone: .accent
                    ),
                    ThreadBlockPlan(
                        kind: .risks,
                        title: "Design Risks",
                        summary: "The biggest risk is overwhelming the user with replayed detail.",
                        items: ["Lore details should not dominate the product model."],
                        tone: .warning
                    ),
                    ThreadBlockPlan(
                        kind: .nextMove,
                        title: "Next Move",
                        summary: "Translate the metaphor into 3 concrete UI rules.",
                        items: [],
                        tone: .accent
                    )
                ],
                primaryAction: "Translate the metaphor into 3 concrete UI rules."
            )
        )

        let update = helper.presentationUpdate(from: llmResult, input: input)
        guard case .applied = update.status else {
            return XCTFail("Expected valid presentation plan")
        }
        guard let presentation = update.presentation else {
            return XCTFail("Expected normalized presentation output")
        }

        XCTAssertEqual(presentation.headline, "Focus the cockpit on the core design tension, not on raw note categories.")
        XCTAssertEqual(presentation.blocks.map(\.kind), [.principles, .risks, .nextMove])
        XCTAssertEqual(presentation.blocks.first?.title, "Design Principles")
        XCTAssertEqual(presentation.primaryAction, "Translate the metaphor into 3 concrete UI rules.")
    }

    func testInvalidAIPresentationPlanReturnsExplicitRejectionReason() {
        let thread = TestFixtures.makeThread(title: "Pensieve design principles")
        let latest = TestFixtures.makeEntry(
            threadID: thread.id,
            text: "Pensieve should externalize memory without replacing judgment."
        )
        let signature = ThreadSignature(
            thread: thread,
            goalStatement: thread.goalLayer.goalStatement,
            coreObjects: [],
            activeClaims: [],
            latestAnchorSummary: nil,
            openLoops: [],
            lastActiveAt: thread.lastActiveAt
        )
        let input = ThreadStateInput(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            signature: signature,
            activeClaims: [],
            questions: [],
            evidenceEntries: [],
            sourceEntries: [],
            recentEntries: [latest],
            recentNotes: [AISnippet(id: latest.id, text: latest.summaryText, kind: latest.kind)],
            anchor: nil,
            entriesSinceAnchor: [latest],
            discourseRelations: []
        )
        let snapshot = helper.synthesizeThreadState(input: input)
        let llmResult = ResumeSynthesisResult(
            currentJudgment: snapshot.currentJudgment,
            openLoops: snapshot.openLoops,
            nextAction: snapshot.nextAction,
            restartNote: snapshot.restartNote,
            recoveryLines: snapshot.recoveryLines,
            resolvedSoFar: snapshot.resolvedSoFar,
            presentationPlan: ThreadPresentationPlan(
                headline: "   ",
                blocks: [],
                primaryAction: nil
            )
        )

        let update = helper.presentationUpdate(from: llmResult, input: input)

        switch update.status {
        case .applied:
            XCTFail("Expected invalid plan status")
        case let .invalid(reason):
            XCTAssertEqual(reason, "AI plan headline was empty.")
        }
        XCTAssertNil(update.presentation)
    }

    func testCompactWorkingReadPrefersHeadingOverWholeBody() {
        let longNote = """
        @J.K.罗琳 亲述的冥想盆

        Pensieve是一种由金属或石头制成的宽而浅的盆子，通常装饰精美或镶嵌宝石，并承载着强大而复杂的魔法咒语。
        感知到的Pensieve的危险与其对记忆或思维的力量有关，可以重现记忆，使其能够重新体验。
        """

        let compact = helper.compactWorkingRead(longNote, maxLength: 88)

        XCTAssertEqual(compact, "@J.K.罗琳 亲述的冥想盆")
        XCTAssertFalse(compact.contains("Pensieve是一种"))
    }
}
