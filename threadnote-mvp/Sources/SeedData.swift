import Foundation

func makeSeedSnapshot() -> AppSnapshot {
    let firstSessionID = UUID()
    let secondSessionID = UUID()
    let thirdSessionID = UUID()
    let buildThread = ThreadRecord(
        id: UUID(),
        title: "Resume should restart work in 10 seconds",
        prompt: "How should Threadnote reopen a thread without forcing the user to reread a wall of context?",
        goalLayer: ThreadGoalLayer(
            goalStatement: "Define a Resume model that lets someone reopen a thread and continue work in 10-30 seconds.",
            goalType: .build,
            successCondition: "The Resume design is clear enough that the thread can restart work with only a tiny visible surface.",
            currentStage: .synthesizing
        ),
        status: .active,
        createdAt: .now.addingTimeInterval(-86_400 * 4),
        updatedAt: .now.addingTimeInterval(-3_600),
        lastActiveAt: .now.addingTimeInterval(-3_600),
        color: .sky
    )
    let studyThread = ThreadRecord(
        id: UUID(),
        title: "Hitchcock suspense pattern study",
        prompt: "What recurring suspense techniques appear across Hitchcock's films?",
        goalLayer: ThreadGoalLayer(
            goalStatement: "Watch Hitchcock films and extract a reusable note on how suspense is staged across multiple works.",
            goalType: .study,
            successCondition: "The thread has a stable pattern read with enough examples and one or two useful exceptions.",
            currentStage: .gathering
        ),
        status: .active,
        createdAt: .now.addingTimeInterval(-86_400 * 6),
        updatedAt: .now.addingTimeInterval(-7_200),
        lastActiveAt: .now.addingTimeInterval(-7_200),
        color: .amber
    )
    let researchThread = ThreadRecord(
        id: UUID(),
        title: "OpenClaw product landscape",
        prompt: "What products are adjacent to OpenClaw, and how should they be grouped?",
        goalLayer: ThreadGoalLayer(
            goalStatement: "Map products adjacent to OpenClaw and identify the most useful comparison axes.",
            goalType: .research,
            successCondition: "The landscape is grouped clearly enough that the next comparison can focus on one missing category.",
            currentStage: .gathering
        ),
        status: .active,
        createdAt: .now.addingTimeInterval(-86_400 * 5),
        updatedAt: .now.addingTimeInterval(-10_800),
        lastActiveAt: .now.addingTimeInterval(-10_800),
        color: .emerald
    )
    let threads = [buildThread, studyThread, researchThread]

    let e1 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .claim,
        body: EntryBody(kind: .text, text: "Resume should reopen a thread with a tiny restart surface, not a component wall.", url: nil, title: nil, details: nil),
        summaryText: "Resume should reopen a thread with a tiny restart surface, not a component wall.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400 * 3),
        sessionID: firstSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.95,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )
    let e2 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .question,
        body: EntryBody(kind: .text, text: "What information belongs behind Resolved So Far instead of the default Resume?", url: nil, title: nil, details: nil),
        summaryText: "What information belongs behind Resolved So Far instead of the default Resume?",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400 * 3 + 1_200),
        sessionID: firstSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 1,
        confidenceScore: 0.82,
        inboxState: "resolved"
    )
    let e3 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .evidence,
        body: EntryBody(kind: .text, text: "Users stall when the Resume opens with too many structured blocks before they know what to do next.", url: nil, title: nil, details: nil),
        summaryText: "Users stall when the Resume opens with too many structured blocks before they know what to do next.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400 * 2),
        sessionID: secondSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.92,
        confidenceScore: 0.88,
        inboxState: "resolved"
    )
    let e4 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .source,
        body: EntryBody(kind: .url, text: nil, url: "https://tape.systems", title: "Tape Systems", details: nil),
        summaryText: "https://tape.systems",
        sourceMetadata: SourceMetadata(title: "Tape Systems", locator: "https://tape.systems", citation: "Reference for append-only context architecture."),
        createdAt: .now.addingTimeInterval(-86_400 + 600),
        sessionID: thirdSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.84,
        confidenceScore: 0.93,
        inboxState: "resolved"
    )
    let e5 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .claim,
        body: EntryBody(kind: .text, text: "The default Resume should show only three recovery lines: state, gap, and next move.", url: nil, title: nil, details: nil),
        summaryText: "The default Resume should show only three recovery lines: state, gap, and next move.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400),
        sessionID: thirdSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 1,
        confidenceScore: 0.93,
        inboxState: "resolved"
    )
    let e6 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .evidence,
        body: EntryBody(kind: .text, text: "Users can restart work faster when the first screen answers state, blocker, and next move within one viewport.", url: nil, title: nil, details: nil),
        summaryText: "Users can restart work faster when the first screen answers state, blocker, and next move within one viewport.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-3_400),
        sessionID: thirdSessionID,
        authorType: "user",
        parentEntryID: e5.id,
        supersedesEntryID: nil,
        importanceScore: 0.76,
        confidenceScore: 0.86,
        inboxState: "resolved"
    )
    let unresolved = Entry(
        id: UUID(),
        threadID: nil,
        kind: .claim,
        body: EntryBody(kind: .text, text: "Watching Hitchcock should produce reusable suspense patterns, not just a watch log.", url: nil, title: nil, details: nil),
        summaryText: "Watching Hitchcock should produce reusable suspense patterns, not just a watch log.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-43_200),
        sessionID: UUID(),
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.76,
        confidenceScore: 0.25,
        inboxState: "unresolved"
    )
    let studySessionID = UUID()
    let s1 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .claim,
        body: EntryBody(kind: .text, text: "Hitchcock often delays release by making the viewer know more than the character on screen.", url: nil, title: nil, details: nil),
        summaryText: "Hitchcock often delays release by making the viewer know more than the character on screen.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400 * 2 + 1_800),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.82,
        confidenceScore: 0.76,
        inboxState: "resolved"
    )
    let s2 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .question,
        body: EntryBody(kind: .text, text: "Does this pattern still hold when suspense comes from mistaken identity rather than physical threat?", url: nil, title: nil, details: nil),
        summaryText: "Does this pattern still hold when suspense comes from mistaken identity rather than physical threat?",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-50_000),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.78,
        confidenceScore: 0.72,
        inboxState: "resolved"
    )
    let s3 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .evidence,
        body: EntryBody(kind: .text, text: "Rear Window sustains suspense by trapping the viewer inside one observational position.", url: nil, title: nil, details: nil),
        summaryText: "Rear Window sustains suspense by trapping the viewer inside one observational position.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-20_000),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.85,
        confidenceScore: 0.8,
        inboxState: "resolved"
    )
    let researchSessionID = UUID()
    let r1 = Entry(
        id: UUID(),
        threadID: researchThread.id,
        kind: .claim,
        body: EntryBody(kind: .text, text: "The OpenClaw-adjacent space seems split between capture tools, visual organizers, and workflow shells.", url: nil, title: nil, details: nil),
        summaryText: "The OpenClaw-adjacent space seems split between capture tools, visual organizers, and workflow shells.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-86_400 * 2 + 3_600),
        sessionID: researchSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.8,
        confidenceScore: 0.74,
        inboxState: "resolved"
    )
    let r2 = Entry(
        id: UUID(),
        threadID: researchThread.id,
        kind: .question,
        body: EntryBody(kind: .text, text: "Which comparison axis matters more here: workflow depth or speed of capture?", url: nil, title: nil, details: nil),
        summaryText: "Which comparison axis matters more here: workflow depth or speed of capture?",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-30_000),
        sessionID: researchSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.77,
        confidenceScore: 0.71,
        inboxState: "resolved"
    )
    let r3 = Entry(
        id: UUID(),
        threadID: researchThread.id,
        kind: .source,
        body: EntryBody(kind: .url, text: nil, url: "https://example.com/openclaw-landscape", title: "OpenClaw Landscape", details: nil),
        summaryText: "https://example.com/openclaw-landscape",
        sourceMetadata: SourceMetadata(title: "OpenClaw Landscape", locator: "https://example.com/openclaw-landscape", citation: "A rough external scan of adjacent products and categories."),
        createdAt: .now.addingTimeInterval(-18_000),
        sessionID: researchSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.74,
        confidenceScore: 0.79,
        inboxState: "resolved"
    )
    // Study thread — image resources (film stills / reference images)
    let s4 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .source,
        body: EntryBody(
            kind: .image,
            text: nil,
            url: "https://upload.wikimedia.org/wikipedia/commons/thumb/7/72/Rear_Window_%281954%29.jpg/1280px-Rear_Window_%281954%29.jpg",
            title: "Rear Window — courtyard still",
            details: "Hitchcock frames the entire film from a single fixed vantage point. The restricted view forces the audience to share Jefferies' helplessness."
        ),
        summaryText: "Rear Window courtyard still — fixed POV as suspense device.",
        sourceMetadata: SourceMetadata(
            title: "Rear Window (1954) — courtyard still",
            locator: "https://upload.wikimedia.org/wikipedia/commons/thumb/7/72/Rear_Window_%281954%29.jpg/1280px-Rear_Window_%281954%29.jpg",
            citation: "Wikimedia Commons / Paramount Pictures"
        ),
        createdAt: .now.addingTimeInterval(-86_400 * 2 + 2_400),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.88,
        confidenceScore: 0.95,
        inboxState: "resolved"
    )
    let s5 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .source,
        body: EntryBody(
            kind: .image,
            text: nil,
            url: "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Vertigo_%281958_film%29_poster.jpg/800px-Vertigo_%281958_film%29_poster.jpg",
            title: "Vertigo (1958) — original poster",
            details: "The spiral motif in the poster directly signals the identity-collapse suspense at the film's core."
        ),
        summaryText: "Vertigo poster — spiral motif foreshadows the mistaken-identity suspense structure.",
        sourceMetadata: SourceMetadata(
            title: "Vertigo (1958) original poster",
            locator: "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Vertigo_%281958_film%29_poster.jpg/800px-Vertigo_%281958_film%29_poster.jpg",
            citation: "Wikimedia Commons / Paramount Pictures"
        ),
        createdAt: .now.addingTimeInterval(-40_000),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.72,
        confidenceScore: 0.9,
        inboxState: "resolved"
    )

    // Study thread — audio resource (Bernard Herrmann score analysis)
    let s6 = Entry(
        id: UUID(),
        threadID: studyThread.id,
        kind: .source,
        body: EntryBody(
            kind: .url,
            text: nil,
            url: "https://www.youtube.com/watch?v=ru59X9_dLuQ",
            title: "Bernard Herrmann — Psycho Suite (1960)",
            details: "Herrmann's all-strings score for Psycho. Hitchcock originally wanted no music for the shower scene. The score is the suspense — silence would have killed it."
        ),
        summaryText: "#source Bernard Herrmann — Psycho Suite. Strings-only score that defines the suspense sound.",
        sourceMetadata: SourceMetadata(
            title: "Bernard Herrmann — Psycho Suite (1960)",
            locator: "https://www.youtube.com/watch?v=ru59X9_dLuQ",
            citation: "YouTube / Hitchcock audio reference for study thread."
        ),
        createdAt: .now.addingTimeInterval(-15_000),
        sessionID: studySessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.79,
        confidenceScore: 0.88,
        inboxState: "resolved"
    )

    // Build thread — design reference image (UI wireframe screenshot)
    let e7 = Entry(
        id: UUID(),
        threadID: buildThread.id,
        kind: .source,
        body: EntryBody(
            kind: .image,
            text: nil,
            url: "https://images.unsplash.com/photo-1586281380349-632531db7ed4?w=800",
            title: "Minimal UI wireframe sketch",
            details: "Reference for the three-line restart surface. The constraint here is fitting state + gap + next move into a single visible block without scrolling."
        ),
        summaryText: "Wireframe sketch — three-line restart surface fitting state, gap, next move in one block.",
        sourceMetadata: SourceMetadata(
            title: "Minimal UI wireframe sketch",
            locator: "https://images.unsplash.com/photo-1586281380349-632531db7ed4?w=800",
            citation: "Unsplash — design reference for Resume surface."
        ),
        createdAt: .now.addingTimeInterval(-86_400 + 1_800),
        sessionID: thirdSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.7,
        confidenceScore: 0.85,
        inboxState: "resolved"
    )

    // Research thread — competitor screenshot (image resource)
    let r4 = Entry(
        id: UUID(),
        threadID: researchThread.id,
        kind: .source,
        body: EntryBody(
            kind: .image,
            text: nil,
            url: "https://images.unsplash.com/photo-1611532736597-de2d4265fba3?w=800",
            title: "Competitor product onboarding screenshot",
            details: "Roam-style bidirectional links. The visual density here is a contrast point for the OpenClaw comparison axis on capture speed vs. workflow depth."
        ),
        summaryText: "Competitor onboarding screenshot — bidirectional links, dense interface, slow capture.",
        sourceMetadata: SourceMetadata(
            title: "Competitor product onboarding screenshot",
            locator: "https://images.unsplash.com/photo-1611532736597-de2d4265fba3?w=800",
            citation: "Unsplash — OpenClaw landscape research reference."
        ),
        createdAt: .now.addingTimeInterval(-25_000),
        sessionID: researchSessionID,
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.75,
        confidenceScore: 0.82,
        inboxState: "resolved"
    )

    // Inbox — unresolved image note (not yet routed)
    let unresolvedImage = Entry(
        id: UUID(),
        threadID: nil,
        kind: .note,
        body: EntryBody(
            kind: .image,
            text: nil,
            url: "https://images.unsplash.com/photo-1518770660439-4636190af475?w=800",
            title: "Circuit board macro",
            details: "Interesting visual for the Threadnote architecture diagram — thread nodes as circuit junctions."
        ),
        summaryText: "Circuit board macro — possible diagram metaphor for thread nodes.",
        sourceMetadata: nil,
        createdAt: .now.addingTimeInterval(-900),
        sessionID: UUID(),
        authorType: "user",
        parentEntryID: nil,
        supersedesEntryID: nil,
        importanceScore: 0.5,
        confidenceScore: 0.3,
        inboxState: "unresolved"
    )

    let entries = [e1, e2, e3, e4, e5, e6, e7, s1, s2, s3, s4, s5, s6, r1, r2, r3, r4, unresolved, unresolvedImage]

    let claims = [
        Claim(
            id: UUID(),
            threadID: buildThread.id,
            originEntryID: e5.id,
            statement: e5.summaryText,
            status: .stable,
            createdAt: .now.addingTimeInterval(-50_000),
            updatedAt: .now.addingTimeInterval(-4_000),
            confidenceScore: 0.87
        ),
        Claim(
            id: UUID(),
            threadID: studyThread.id,
            originEntryID: s1.id,
            statement: s1.summaryText,
            status: .working,
            createdAt: .now.addingTimeInterval(-48_000),
            updatedAt: .now.addingTimeInterval(-18_000),
            confidenceScore: 0.76
        ),
        Claim(
            id: UUID(),
            threadID: researchThread.id,
            originEntryID: r1.id,
            statement: r1.summaryText,
            status: .working,
            createdAt: .now.addingTimeInterval(-40_000),
            updatedAt: .now.addingTimeInterval(-12_000),
            confidenceScore: 0.73
        )
    ]

    let anchors = [
        Anchor(
            id: UUID(),
            threadID: buildThread.id,
            createdAt: .now.addingTimeInterval(-3_600),
            basedOnEntryID: e5.id,
            title: "Checkpoint",
            coreQuestion: buildThread.goalLayer.goalStatement,
            stateSummary: "The direction is to make Resume a tiny restart point instead of a structured reading surface.",
            openLoops: [
                "What belongs behind Resolved So Far instead of the default Resume?",
                "How should the second line describe the real blocker instead of repeating a raw question?"
            ],
            nextSteps: [
                "Rewrite the default Resume around three recovery lines: current direction, blocker, and next decision.",
                "Keep all other structure behind one controlled expansion path."
            ],
            claimIDs: claims.filter { $0.threadID == buildThread.id }.map(\.id),
            evidenceEntryIDs: [e3.id, e4.id, e6.id],
            phase: "synthesizing"
        ),
        Anchor(
            id: UUID(),
            threadID: studyThread.id,
            createdAt: .now.addingTimeInterval(-7_200),
            basedOnEntryID: s3.id,
            title: "Checkpoint",
            coreQuestion: studyThread.goalLayer.goalStatement,
            stateSummary: "The current read is that Hitchcock sustains suspense by controlling what the viewer sees and when release is delayed.",
            openLoops: [
                "Does mistaken-identity suspense break the current pattern?",
                "Which film should serve as the strongest counterexample?"
            ],
            nextSteps: [
                "Watch one film that stresses mistaken identity and write the next observation immediately after."
            ],
            claimIDs: claims.filter { $0.threadID == studyThread.id }.map(\.id),
            evidenceEntryIDs: [s3.id],
            phase: "gathering"
        ),
        Anchor(
            id: UUID(),
            threadID: researchThread.id,
            createdAt: .now.addingTimeInterval(-10_800),
            basedOnEntryID: r1.id,
            title: "Checkpoint",
            coreQuestion: researchThread.goalLayer.goalStatement,
            stateSummary: "The map is starting to separate capture tools, visual organizers, and workflow shells, but the comparison axes are still weak.",
            openLoops: [
                "Which missing category matters most for the next comparison pass?",
                "Should workflow depth outrank capture speed as the main axis?"
            ],
            nextSteps: [
                "Probe one missing category before refining the comparison axes."
            ],
            claimIDs: claims.filter { $0.threadID == researchThread.id }.map(\.id),
            evidenceEntryIDs: [r3.id],
            phase: "gathering"
        )
    ]

    let tasks = [
        ThreadTask(
            id: UUID(),
            threadID: buildThread.id,
            originEntryID: nil,
            title: "Refine the three-line recovery surface",
            status: "open",
            createdAt: .now.addingTimeInterval(-1_800),
            updatedAt: .now.addingTimeInterval(-1_800)
        )
    ]

    return AppSnapshot(
        sampleDataVersion: 8,
        threads: threads,
        entries: entries,
        claims: claims,
        anchors: anchors,
        tasks: tasks,
        discourseRelations: []
    )
}
