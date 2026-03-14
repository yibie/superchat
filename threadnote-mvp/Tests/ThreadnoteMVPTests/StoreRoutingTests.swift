import XCTest
@testable import ThreadnoteMVP

@MainActor
final class StoreRoutingTests: XCTestCase {
    func testSubmitCaptureWithoutBackendStaysInInboxAndPrepareShowsNotConfigured() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let snapshot = TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook checklist"),
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook blockers")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ],
            anchors: [
                TestFixtures.makeAnchor(
                    threadID: thread.id,
                    coreQuestion: thread.goalLayer.goalStatement,
                    stateSummary: "Atlas release playbook blockers and next actions"
                )
            ]
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)

        store.quickCaptureDraft.text = "Atlas release playbook"
        store.submitCapture()

        let unresolvedEntry = try XCTUnwrap(store.entries.first(where: { $0.summaryText == "Atlas release playbook" }))
        XCTAssertNil(unresolvedEntry.threadID)
        XCTAssertEqual(unresolvedEntry.inboxState, "unresolved")
        XCTAssertEqual(store.routeDebug(for: unresolvedEntry)?.status, .notConfigured)

        store.openThread(thread.id)
        let threadState = try XCTUnwrap(store.threadState(for: thread.id))
        XCTAssertEqual(threadState.contentState.status, .notConfigured)
        XCTAssertTrue(threadState.presentation.blocks.isEmpty)

        store.prepareView(type: .writing, for: thread.id)
        XCTAssertEqual(store.preparedView?.threadID, thread.id)
        XCTAssertEqual(store.preparedView?.contentState.status, .notConfigured)
        XCTAssertTrue(store.preparedView?.recommendedNextSteps.isEmpty ?? false)
    }

    func testSubmitCaptureAutoRoutesWithLLMBackendAndPrepareViewWorks() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let snapshot = TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook checklist"),
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook blockers")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ],
            anchors: [
                TestFixtures.makeAnchor(
                    threadID: thread.id,
                    coreQuestion: thread.goalLayer.goalStatement,
                    stateSummary: "Atlas release playbook blockers and next actions"
                )
            ]
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)
        let backend = MockAIBackend()
        backend.routeHandler = { request in
            RoutePlanningResult(
                shouldRoute: true,
                selectedThreadID: thread.id,
                decisionReason: "This note clearly extends the Atlas release thread.",
                suggestions: [
                    RoutePlanningSuggestion(threadID: thread.id, reason: "Release playbook wording matches the existing thread.")
                ],
                debugPayload: nil
            )
        }
        store.setAIBackendForTesting(backend)

        store.quickCaptureDraft.text = "Atlas release playbook"
        store.submitCapture()

        XCTAssertTrue(waitUntil {
            store.entries.first(where: { $0.summaryText == "Atlas release playbook" })?.threadID == thread.id
        })

        let routedEntry = try XCTUnwrap(store.entries.first(where: { $0.summaryText == "Atlas release playbook" }))
        XCTAssertEqual(routedEntry.threadID, thread.id)
        XCTAssertEqual(routedEntry.inboxState, "resolved")

        store.openThread(thread.id)
        _ = try XCTUnwrap(store.threadState(for: thread.id))
        XCTAssertTrue(waitUntil {
            store.threadState(for: thread.id)?.contentState.status == .ready
        })

        store.prepareView(type: .writing, for: thread.id)
        XCTAssertEqual(store.preparedView?.threadID, thread.id)
        XCTAssertTrue(waitUntil {
            store.preparedView?.contentState.status == .ready
        })
        XCTAssertFalse(store.preparedView?.recommendedNextSteps.isEmpty ?? true)
    }

    func testManualAndAutoRoutingShareClaimPromotionAndWorkingMemorySideEffects() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let anchor = TestFixtures.makeAnchor(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            stateSummary: "Atlas release playbook blockers and next actions"
        )
        let unresolvedClaimEntry = TestFixtures.makeEntry(
            threadID: nil,
            kind: .claim,
            text: "Atlas release playbook",
            inboxState: "unresolved"
        )

        let manualStore = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [unresolvedClaimEntry],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ],
            anchors: [anchor]
        ))
        let manualEntry = try XCTUnwrap(manualStore.inboxEntries.first)
        manualStore.resolveInboxEntry(manualEntry, to: thread.id)

        XCTAssertTrue(manualStore.inboxEntries.isEmpty)
        XCTAssertEqual(manualStore.claims(for: thread.id).count, 2)
        XCTAssertTrue(waitUntil {
            manualStore.memoryRecords(for: thread.id, scope: .working).count == 1
        })

        let autoStore = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook checklist"),
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook blockers")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ],
            anchors: [anchor]
        ))
        let backend = MockAIBackend()
        backend.routeHandler = { request in
            RoutePlanningResult(
                shouldRoute: true,
                selectedThreadID: thread.id,
                decisionReason: "The claim matches the Atlas release thread.",
                suggestions: [
                    RoutePlanningSuggestion(threadID: thread.id, reason: "Claim wording directly matches the existing thread.")
                ],
                debugPayload: nil
            )
        }
        autoStore.setAIBackendForTesting(backend)
        autoStore.quickCaptureDraft.text = "#claim Atlas release playbook"
        autoStore.submitCapture()

        XCTAssertTrue(waitUntil {
            autoStore.entries.first(where: { $0.summaryText == "Atlas release playbook" })?.threadID == thread.id
        })

        let capturedEntry = try XCTUnwrap(autoStore.entries.first(where: { $0.kind == .claim }))
        XCTAssertEqual(capturedEntry.summaryText, "Atlas release playbook")
        XCTAssertTrue(autoStore.inboxEntries.isEmpty)
        XCTAssertEqual(autoStore.claims(for: thread.id).count, 2)
        XCTAssertTrue(waitUntil {
            autoStore.memoryRecords(for: thread.id, scope: .working).count == 1
        })
        XCTAssertEqual(
            autoStore.entries.first(where: { $0.summaryText == "Atlas release playbook" })?.threadID,
            thread.id
        )
    }

    func testAmbiguousCaptureStaysInInbox() throws {
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
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)
        let backend = MockAIBackend()
        backend.routeHandler = { _ in
            RoutePlanningResult(
                shouldRoute: false,
                selectedThreadID: nil,
                decisionReason: "The note is ambiguous between two similarly named Atlas threads.",
                suggestions: [],
                debugPayload: nil
            )
        }
        store.setAIBackendForTesting(backend)

        store.quickCaptureDraft.text = "Atlas weekly plan"
        store.submitCapture()

        XCTAssertTrue(waitUntil {
            store.entries.contains(where: { $0.summaryText == "Atlas weekly plan" })
        })
        let unresolved = try XCTUnwrap(store.inboxEntries.first(where: { $0.summaryText == "Atlas weekly plan" }))
        XCTAssertTrue(waitUntil {
            store.routeDebug(for: unresolved)?.status == .stayedInInbox
        })
        XCTAssertNil(unresolved.threadID)
        XCTAssertEqual(unresolved.inboxState, "unresolved")
        let debug = try XCTUnwrap(store.routeDebug(for: unresolved))
        XCTAssertEqual(debug.status, .stayedInInbox)
        XCTAssertEqual(debug.decisionReason, "The note is ambiguous between two similarly named Atlas threads.")
        XCTAssertEqual(debug.topCandidates.count, 2)
    }

    func testCaptureShowsRouteFailureWhenBackendIsUnreachable() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let snapshot = TestFixtures.makeSnapshot(
            threads: [thread],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ]
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)
        let backend = MockAIBackend()
        backend.routeHandler = { _ in
            throw URLError(.cannotConnectToHost)
        }
        store.setAIBackendForTesting(backend)

        store.quickCaptureDraft.text = "Atlas release playbook"
        store.submitCapture()

        let unresolved = try XCTUnwrap(store.entries.first(where: { $0.summaryText == "Atlas release playbook" }))
        XCTAssertTrue(waitUntil {
            store.routeDebug(for: unresolved)?.status == .failed
        })

        let debug = try XCTUnwrap(store.routeDebug(for: unresolved))
        XCTAssertEqual(debug.status, .failed)
        XCTAssertEqual(debug.connectivityStatus, "failed")
        XCTAssertFalse(debug.connectivityMessage?.isEmpty ?? true)
        XCTAssertNil(unresolved.threadID)
        XCTAssertEqual(unresolved.inboxState, "unresolved")
    }

    func testThreadStateProducesStructuredRestartSnapshot() throws {
        let thread = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let evidence = TestFixtures.makeEntry(
            threadID: thread.id,
            kind: .evidence,
            text: "Usage-based pricing matches how Atlas customers evaluate spend."
        )
        let snapshot = TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [evidence],
            claims: [
                TestFixtures.makeClaim(
                    threadID: thread.id,
                    statement: "Atlas pricing should stay usage-based",
                    status: .stable
                )
            ]
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)
        store.setAIBackendForTesting(MockAIBackend())

        _ = try XCTUnwrap(store.threadState(for: thread.id))
        XCTAssertTrue(waitUntil {
            store.threadState(for: thread.id)?.contentState.status == .ready
        })
        let state = try XCTUnwrap(store.threadState(for: thread.id))

        XCTAssertFalse(state.presentation.headline.isEmpty)
        XCTAssertEqual(state.presentation.blocks.first?.kind, .judgment)
        XCTAssertTrue(state.presentation.blocks.contains(where: { $0.kind == .gap }))
        XCTAssertTrue(state.presentation.blocks.contains(where: { $0.kind == .nextMove }))
        XCTAssertFalse(state.judgmentBasis.isEmpty)
        XCTAssertEqual(
            state.openLoops.first,
            "The strongest evidence is still ungrounded by a source or reference."
        )
    }

    func testThreadStateShowsNotConfiguredStateInsteadOfDeterministicFallback() throws {
        let thread = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread]
        ))

        let state = try XCTUnwrap(store.threadState(for: thread.id))

        XCTAssertEqual(state.contentState.status, .notConfigured)
        XCTAssertEqual(state.aiDebug.status, .notConfigured)
        XCTAssertTrue(state.aiDebug.message.contains("not configured"))
        XCTAssertTrue(state.presentation.blocks.isEmpty)
        XCTAssertTrue(state.restartNote.isEmpty)
    }

    func testReferenceCompletionCandidatesIncludeOnlyTopLevelNotes() throws {
        let thread = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let newerEntry = TestFixtures.makeEntry(
            threadID: thread.id,
            text: "Newest note",
            createdAt: Date(timeIntervalSince1970: 200)
        )
        let reply = Entry(
            id: UUID(),
            threadID: thread.id,
            kind: .note,
            body: EntryBody(kind: .text, text: "Reply note", url: nil, title: nil, details: nil),
            summaryText: "Reply note",
            sourceMetadata: nil,
            createdAt: Date(timeIntervalSince1970: 300),
            sessionID: nil,
            authorType: "user",
            parentEntryID: newerEntry.id,
            supersedesEntryID: nil,
            importanceScore: nil,
            confidenceScore: nil,
            inboxState: "resolved"
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [
                TestFixtures.makeEntry(
                    threadID: thread.id,
                    text: "Older note",
                    createdAt: Date(timeIntervalSince1970: 100)
                ),
                newerEntry,
                reply
            ]
        ))

        let candidates = store.referenceCompletionCandidates

        XCTAssertEqual(candidates.map(\.0), ["Newest note", "Older note"])
        XCTAssertTrue(candidates.allSatisfy { $0.2 == "note.text" })
        XCTAssertFalse(candidates.contains(where: { $0.0 == thread.title }))
        XCTAssertFalse(candidates.contains(where: { $0.0 == "Reply note" }))
    }

    func testAutoRoutedCaptureKeepsRouteDebugSnapshot() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let snapshot = TestFixtures.makeSnapshot(
            threads: [thread],
            entries: [
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook checklist"),
                TestFixtures.makeEntry(threadID: thread.id, text: "Atlas release playbook blockers")
            ],
            claims: [
                TestFixtures.makeClaim(threadID: thread.id, statement: "Atlas release playbook")
            ]
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: snapshot)
        let backend = MockAIBackend()
        backend.routeHandler = { _ in
            RoutePlanningResult(
                shouldRoute: true,
                selectedThreadID: thread.id,
                decisionReason: "This note belongs to the Atlas launch thread.",
                suggestions: [
                    RoutePlanningSuggestion(threadID: thread.id, reason: "The language directly matches the launch thread.")
                ],
                debugPayload: nil
            )
        }
        store.setAIBackendForTesting(backend)

        store.quickCaptureDraft.text = "Atlas release playbook"
        store.submitCapture()

        XCTAssertTrue(waitUntil {
            store.entries.first(where: { $0.summaryText == "Atlas release playbook" })?.threadID == thread.id
        })
        let routedEntry = try XCTUnwrap(store.entries.first(where: { $0.summaryText == "Atlas release playbook" }))
        XCTAssertTrue(waitUntil {
            store.routeDebug(for: routedEntry)?.status == .routed
        })
        let debug = try XCTUnwrap(store.routeDebug(for: routedEntry))
        XCTAssertEqual(debug.status, .routed)
        XCTAssertEqual(debug.selectedThreadID, thread.id)
        XCTAssertEqual(debug.selectedThreadTitle, thread.title)
        XCTAssertFalse(debug.topCandidates.isEmpty)
    }

    func testThreadStateDoesNotInlineWholeLongCheckpointBody() throws {
        let thread = TestFixtures.makeThread(title: "Pensieve design")
        let anchor = TestFixtures.makeAnchor(
            threadID: thread.id,
            coreQuestion: thread.goalLayer.goalStatement,
            stateSummary: """
            @J.K.罗琳 亲述的冥想盆

            Pensieve是一种由金属或石头制成的宽而浅的盆子，通常装饰精美或镶嵌宝石，并承载着强大而复杂的魔法咒语。
            感知到的Pensieve的危险与其对记忆或思维的力量有关，可以重现记忆。
            """
        )
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread],
            anchors: [anchor]
        ))
        store.setAIBackendForTesting(MockAIBackend())

        _ = try XCTUnwrap(store.threadState(for: thread.id))
        XCTAssertTrue(waitUntil {
            store.threadState(for: thread.id)?.contentState.status == .ready
        })
        let state = try XCTUnwrap(store.threadState(for: thread.id))

        XCTAssertTrue(state.restartNote.contains("AI restart note"))
        XCTAssertFalse(state.presentation.headline.contains("Pensieve是一种由金属或石头制成的宽而浅的盆子"))
        XCTAssertEqual(state.currentJudgment, "@J.K.罗琳 亲述的冥想盆")
    }

    func testThreadStateShowsResumeErrorWhenBackendCallFails() throws {
        let thread = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread]
        ))
        let backend = MockAIBackend()
        backend.resumeHandler = { _ in
            throw URLError(.cannotConnectToHost)
        }
        store.setAIBackendForTesting(backend)

        _ = try XCTUnwrap(store.threadState(for: thread.id))

        XCTAssertTrue(waitUntil {
            store.threadState(for: thread.id)?.contentState.status == .error
        })

        let state = try XCTUnwrap(store.threadState(for: thread.id))
        XCTAssertEqual(state.contentState.status, .error)
        XCTAssertEqual(state.aiDebug.status, .failed)
        XCTAssertTrue(state.presentation.blocks.isEmpty)
    }

    func testPrepareViewShowsErrorWhenBackendCallFails() throws {
        let thread = TestFixtures.makeThread(title: "Atlas launch thread")
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [thread]
        ))
        let backend = MockAIBackend()
        backend.draftHandler = { _ in
            throw URLError(.networkConnectionLost)
        }
        store.setAIBackendForTesting(backend)

        store.prepareView(type: .writing, for: thread.id)

        XCTAssertTrue(waitUntil {
            store.preparedView?.contentState.status == .error
        })

        XCTAssertEqual(store.preparedView?.contentState.status, .error)
        XCTAssertTrue(store.preparedView?.recommendedNextSteps.isEmpty ?? false)
    }

    func testSwitchingThreadsCancelsPendingResumeAndAllowsRetry() throws {
        let atlas = TestFixtures.makeThread(title: "Atlas pricing strategy")
        let pensieve = TestFixtures.makeThread(title: "Pensieve design")
        let store = try TestFixtures.makeThreadnoteStore(snapshot: TestFixtures.makeSnapshot(
            threads: [atlas, pensieve]
        ))

        let backend = MockAIBackend()
        var resumeCallCount = 0
        backend.resumeHandler = { request in
            resumeCallCount += 1
            if resumeCallCount == 1 {
                try await Task.sleep(for: .milliseconds(200))
            }
            return ResumeSynthesisResult(
                currentJudgment: request.currentJudgment,
                openLoops: request.openLoops,
                nextAction: request.nextAction,
                restartNote: "AI restart note: \(request.coreQuestion)",
                recoveryLines: request.recoveryLines,
                resolvedSoFar: request.resolvedSoFar,
                presentationPlan: ThreadPresentationPlan(
                    headline: "AI restart note for \(request.coreQuestion)",
                    blocks: [
                        ThreadBlockPlan(
                            kind: .judgment,
                            title: "Current Judgment",
                            summary: request.currentJudgment,
                            items: [request.judgmentBasis].filter { !$0.isEmpty },
                            tone: .accent
                        )
                    ],
                    primaryAction: request.nextAction
                ),
                debugPayload: nil
            )
        }
        store.setAIBackendForTesting(backend)

        store.openThread(atlas.id)
        _ = try XCTUnwrap(store.threadState(for: atlas.id))

        store.openThread(pensieve.id)
        store.openThread(atlas.id)
        _ = try XCTUnwrap(store.threadState(for: atlas.id))

        XCTAssertTrue(waitUntil(timeout: 0.8) {
            store.threadState(for: atlas.id)?.contentState.status == .ready
        })
        XCTAssertGreaterThanOrEqual(resumeCallCount, 2)
    }

    private func waitUntil(
        timeout: TimeInterval = 0.4,
        pollInterval: TimeInterval = 0.02,
        condition: () -> Bool
    ) -> Bool {
        let deadline = Date().addingTimeInterval(timeout)
        while Date() < deadline {
            if condition() {
                return true
            }
            RunLoop.current.run(until: Date().addingTimeInterval(pollInterval))
        }
        return condition()
    }
}
