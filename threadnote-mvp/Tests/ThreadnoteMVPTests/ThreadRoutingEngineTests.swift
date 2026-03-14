import XCTest
@testable import ThreadnoteMVP

final class ThreadRoutingEngineTests: XCTestCase {
    func testSuggestAndDecideShareTopThread() throws {
        let now = Date()
        let atlas = TestFixtures.makeThread(title: "Atlas release", lastActiveAt: now)
        let hiring = TestFixtures.makeThread(title: "Hiring plan", lastActiveAt: now.addingTimeInterval(-60))
        let snapshot = TestFixtures.makeSnapshot(
            threads: [atlas, hiring],
            entries: [
                TestFixtures.makeEntry(threadID: atlas.id, text: "Atlas release playbook checklist"),
                TestFixtures.makeEntry(threadID: atlas.id, text: "Atlas release playbook blockers"),
                TestFixtures.makeEntry(threadID: hiring.id, text: "Hiring debrief notes")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: atlas.id, statement: "Atlas release playbook"),
                TestFixtures.makeClaim(threadID: hiring.id, statement: "Hiring interview rubric")
            ],
            anchors: [
                TestFixtures.makeAnchor(
                    threadID: atlas.id,
                    coreQuestion: atlas.goalLayer.goalStatement,
                    stateSummary: "Atlas release blockers and next launch steps"
                )
            ]
        )
        let engine = try makeEngine(snapshot: snapshot)

        let suggestions = engine.suggest(noteText: "Atlas release playbook", limit: 3)
        guard let topSuggestion = suggestions.first else {
            return XCTFail("Expected a thread suggestion")
        }
        XCTAssertEqual(topSuggestion.thread.id, atlas.id)

        guard case let .route(threadID, score, _) = engine.decide(noteText: "Atlas release playbook") else {
            return XCTFail("Expected a confident routing decision")
        }
        XCTAssertEqual(threadID, atlas.id)
        XCTAssertEqual(score, topSuggestion.score)
    }

    func testSuggestFallsBackToRecencyButDecisionDoesNotAutoRouteWithoutMatches() throws {
        let now = Date()
        let recent = TestFixtures.makeThread(title: "Recent thread", lastActiveAt: now)
        let older = TestFixtures.makeThread(title: "Older thread", lastActiveAt: now.addingTimeInterval(-600))
        let snapshot = TestFixtures.makeSnapshot(
            threads: [recent, older],
            claims: [
                TestFixtures.makeClaim(threadID: recent.id, statement: "Atlas planning notes"),
                TestFixtures.makeClaim(threadID: older.id, statement: "Hiring panel notes")
            ]
        )
        let engine = try makeEngine(snapshot: snapshot)

        let suggestions = engine.suggest(noteText: "quantum banana mismatch", limit: 2)
        XCTAssertEqual(suggestions.map(\.thread.id), [recent.id, older.id])
        XCTAssertEqual(Set(suggestions.map(\.reason)), Set(["Recently active"]))

        guard case let .noMatch(reason) = engine.decide(noteText: "quantum banana mismatch") else {
            return XCTFail("Expected no auto-route for unrelated text")
        }
        XCTAssertFalse(reason.isEmpty)
    }

    func testDecisionRequiresSeparationFromSecondCandidate() throws {
        let now = Date()
        let atlasA = TestFixtures.makeThread(title: "Atlas alpha", lastActiveAt: now)
        let atlasB = TestFixtures.makeThread(title: "Atlas beta", lastActiveAt: now.addingTimeInterval(-60))
        let snapshot = TestFixtures.makeSnapshot(
            threads: [atlasA, atlasB],
            claims: [
                TestFixtures.makeClaim(threadID: atlasA.id, statement: "Atlas weekly plan"),
                TestFixtures.makeClaim(threadID: atlasB.id, statement: "Atlas weekly plan")
            ]
        )
        let engine = try makeEngine(snapshot: snapshot)

        guard case .noMatch = engine.decide(noteText: "Atlas weekly plan") else {
            return XCTFail("Expected no decision when top candidates are too close")
        }
    }

    func testStableClaimsAndAnchorsBoostRelatedThreadRanking() throws {
        let now = Date()
        let boosted = TestFixtures.makeThread(title: "Atlas research", lastActiveAt: now)
        let plain = TestFixtures.makeThread(title: "Atlas notes", lastActiveAt: now.addingTimeInterval(-120))
        let snapshot = TestFixtures.makeSnapshot(
            threads: [boosted, plain],
            entries: [
                TestFixtures.makeEntry(threadID: boosted.id, text: "Atlas release checklist"),
                TestFixtures.makeEntry(threadID: plain.id, text: "Atlas release")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: boosted.id, statement: "Atlas release playbook"),
                TestFixtures.makeClaim(threadID: plain.id, statement: "Atlas release")
            ],
            anchors: [
                TestFixtures.makeAnchor(
                    threadID: boosted.id,
                    coreQuestion: boosted.goalLayer.goalStatement,
                    stateSummary: "Atlas release playbook and rollout checklist"
                )
            ]
        )
        let engine = try makeEngine(snapshot: snapshot)

        let suggestions = engine.suggest(noteText: "Atlas release playbook checklist", limit: 2)
        XCTAssertEqual(suggestions.first?.thread.id, boosted.id)
        XCTAssertTrue((suggestions.first?.score ?? 0) > (suggestions.dropFirst().first?.score ?? 0))
    }

    func testThreadSignaturesCanRouteWithoutRetrievalDocuments() throws {
        let pricing = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let hiring = TestFixtures.makeThread(title: "Hiring plan")
        let signatureClaims = [
            TestFixtures.makeClaim(threadID: pricing.id, statement: "OpenAI should keep Atlas pricing usage-based")
        ]
        let signatureAnchors = [
            TestFixtures.makeAnchor(
                threadID: pricing.id,
                coreQuestion: pricing.goalLayer.goalStatement,
                stateSummary: "OpenAI pricing options for Atlas launch"
            )
        ]

        let persistence = try TestFixtures.makePersistenceStore(snapshot: TestFixtures.makeSnapshot(
            threads: [pricing, hiring]
        ))
        let engine = ThreadRoutingEngine(
            retrievalEngine: RetrievalEngine(pool: persistence.databasePool),
            threadsProvider: { [pricing, hiring] in [pricing, hiring] },
            entriesProvider: { _ in [] },
            claimsProvider: { threadID in
                signatureClaims.filter { $0.threadID == threadID }
            },
            latestAnchorProvider: { threadID in
                signatureAnchors.first { $0.threadID == threadID }
            }
        )

        guard case let .route(threadID, score, reason) = engine.decide(
            noteText: "OpenAI Atlas pricing should stay usage-based"
        ) else {
            return XCTFail("Expected signature-driven routing decision")
        }
        XCTAssertEqual(threadID, pricing.id)
        XCTAssertGreaterThanOrEqual(score, 20)
        XCTAssertFalse(reason.isEmpty)
    }

    func testDebugStateExplainsWhyAmbiguousCaptureStayedInInbox() throws {
        let now = Date()
        let atlasA = TestFixtures.makeThread(title: "Atlas alpha", lastActiveAt: now)
        let atlasB = TestFixtures.makeThread(title: "Atlas beta", lastActiveAt: now.addingTimeInterval(-60))
        let snapshot = TestFixtures.makeSnapshot(
            threads: [atlasA, atlasB],
            claims: [
                TestFixtures.makeClaim(threadID: atlasA.id, statement: "Atlas weekly plan"),
                TestFixtures.makeClaim(threadID: atlasB.id, statement: "Atlas weekly plan")
            ]
        )
        let engine = try makeEngine(snapshot: snapshot)

        let debug = engine.debugState(noteText: "Atlas weekly plan", limit: 2)

        XCTAssertEqual(debug.supportEngineLabel, "Deterministic routing engine")
        XCTAssertEqual(debug.status, .stayedInInbox)
        XCTAssertEqual(debug.decisionReason, "Top thread is not separated enough from the next candidate.")
        XCTAssertEqual(debug.topCandidates.count, 2)
        XCTAssertEqual(debug.topCandidates.map(\.threadID), [atlasA.id, atlasB.id])
        XCTAssertEqual(debug.topScore, debug.secondScore)
    }

    private func makeEngine(snapshot: AppSnapshot) throws -> ThreadRoutingEngine {
        let store = try TestFixtures.makePersistenceStore(snapshot: snapshot)
        return ThreadRoutingEngine(
            retrievalEngine: RetrievalEngine(pool: store.databasePool),
            threadsProvider: { snapshot.threads },
            entriesProvider: { threadID in
                snapshot.entries.filter { $0.threadID == threadID }
            },
            claimsProvider: { threadID in
                snapshot.claims.filter { $0.threadID == threadID }
            },
            latestAnchorProvider: { threadID in
                snapshot.anchors
                    .filter { $0.threadID == threadID }
                    .max(by: { $0.createdAt < $1.createdAt })
            }
        )
    }
}
