import Foundation

// MARK: - Onboarding snapshot
// Shown on first launch (empty database). Two threads:
//   1. "Getting started" — teaches the core loop by example
//   2. "My first thread" — a blank thread ready for the user's own question
// No fake research data. Every note explains what it is and why.

func makeSeedSnapshot() -> AppSnapshot {
    let sessionID = UUID()
    let now = Date()

    // MARK: Thread 1 — guide
    let guideThread = ThreadRecord(
        id: UUID(),
        title: "Getting started with Threadnote",
        prompt: "What is Threadnote for, and how do I use it effectively?",
        goalLayer: ThreadGoalLayer(
            goalStatement: "Understand the core loop: capture → thread → resume.",
            goalType: .study,
            successCondition: "You can open a thread, add a note, and resume work the next day without losing context.",
            currentStage: .gathering
        ),
        status: .active,
        createdAt: now.addingTimeInterval(-3600),
        updatedAt: now.addingTimeInterval(-60),
        lastActiveAt: now.addingTimeInterval(-60),
        color: .sky
    )

    // MARK: Thread 2 — blank starter
    let blankThread = ThreadRecord(
        id: UUID(),
        title: "My first thread",
        prompt: "What problem or question do you want to return to?",
        goalLayer: ThreadGoalLayer(
            goalStatement: "Replace this with your own question.",
            goalType: .research,
            successCondition: "",
            currentStage: .gathering
        ),
        status: .active,
        createdAt: now.addingTimeInterval(-1800),
        updatedAt: now.addingTimeInterval(-1800),
        lastActiveAt: now.addingTimeInterval(-1800),
        color: .amber
    )

    // MARK: Guide entries
    let g1 = Entry(
        id: UUID(),
        threadID: guideThread.id,
        kind: .note,
        body: EntryBody(kind: .text, text: "Threadnote is built around one idea: thinking is interrupted. A thread is a place where you can always pick up where you left off.", url: nil, title: nil, details: nil),
        summaryText: "Threadnote is built around one idea: thinking is interrupted. A thread is a place where you can always pick up where you left off.",
        sourceMetadata: nil,
        createdAt: now.addingTimeInterval(-3500),
        sessionID: sessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.9,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )
    let g2 = Entry(
        id: UUID(),
        threadID: guideThread.id,
        kind: .note,
        body: EntryBody(kind: .text, text: "The Restart Note at the top of each thread is generated automatically. It summarises where you left off. You don't write it — the system does.", url: nil, title: nil, details: nil),
        summaryText: "The Restart Note at the top of each thread is generated automatically. It summarises where you left off. You don't write it — the system does.",
        sourceMetadata: nil,
        createdAt: now.addingTimeInterval(-3400),
        sessionID: sessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.85,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )
    let g3 = Entry(
        id: UUID(),
        threadID: guideThread.id,
        kind: .note,
        body: EntryBody(kind: .text, text: "Capture a note from anywhere using ⌘N. Notes captured without a thread go to the Inbox. AI will suggest the right thread automatically — you only need to correct it when it's wrong.", url: nil, title: nil, details: nil),
        summaryText: "Capture a note from anywhere using ⌘N. Notes without a thread go to the Inbox. AI routes them automatically.",
        sourceMetadata: nil,
        createdAt: now.addingTimeInterval(-3300),
        sessionID: sessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.8,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )
    let g4 = Entry(
        id: UUID(),
        threadID: guideThread.id,
        kind: .claim,
        body: EntryBody(kind: .text, text: "A thread works best when it has a single, specific question at its core — not a broad topic.", url: nil, title: nil, details: nil),
        summaryText: "A thread works best when it has a single, specific question at its core — not a broad topic.",
        sourceMetadata: nil,
        createdAt: now.addingTimeInterval(-3200),
        sessionID: sessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.9,
        confidenceScore: 0.85,
        inboxState: "resolved"
    )
    let g5 = Entry(
        id: UUID(),
        threadID: guideThread.id,
        kind: .note,
        body: EntryBody(kind: .text, text: "When you're ready to write or decide, open Thread Tools → Prepare View. This pulls together your claims, open loops, and next steps into one surface.", url: nil, title: nil, details: nil),
        summaryText: "When you're ready to write or decide, open Thread Tools → Prepare View.",
        sourceMetadata: nil,
        createdAt: now.addingTimeInterval(-3100),
        sessionID: sessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.75,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )

    let claim = Claim(
        id: UUID(),
        threadID: guideThread.id,
        originEntryID: g4.id,
        statement: "A thread works best when it has a single, specific question at its core — not a broad topic.",
        status: .working,
        createdAt: now.addingTimeInterval(-3200),
        updatedAt: now.addingTimeInterval(-3200),
        confidenceScore: 0.85
    )

    return AppSnapshot(
        sampleDataVersion: 9,
        threads: [guideThread, blankThread],
        entries: [g1, g2, g3, g4, g5],
        claims: [claim],
        anchors: [],
        tasks: [],
        discourseRelations: []
    )
}
