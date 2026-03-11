# PR/FAQ Draft: Native App Foundation

> 状态：基于现有对话与文档推导的首稿，用于开启本阶段；后续可在 phase 内迭代修订。

## Press Release

### Headline
Threadnote brings interrupted thinking back into a native Mac workspace.

### Subtitle
Instead of storing more notes, Threadnote remembers the last useful state of a problem and helps you continue from there.

### Date
March 10, 2026

### Intro paragraph
Today we are introducing the next phase of Threadnote: a native macOS workspace for people who do serious thinking across days or weeks. Threadnote turns captures, sources, evidence, and claims into resumable problem spaces, so users can return to a thread and continue immediately instead of reopening an archive.

### Problem paragraph
People who work through complex ideas do not mainly suffer from a lack of storage. They suffer from broken continuity. Existing note tools are good at collecting text, links, and files, but weak at preserving where a line of thinking stopped, what remains unresolved, and what should happen next. As note collections grow, capture becomes easier than continuation.

### Solution paragraph
Threadnote solves this by treating a thread as a recoverable problem space rather than a folder or long document. Captures flow in quickly, the system structures them into questions, claims, evidence, and sources, and AI helps restore working context instead of replacing the author. The next phase upgrades the MVP into a real Mac app with reliable input, a native editor stack, a calmer Craft-like workspace, and an AI layer focused on classification, structuring, and resume generation.

### Company leader quote
"We are not building another note database. We are building a workspace that lets people come back to a hard problem and feel their thinking is still alive." said the Threadnote team.

### How the product works
Users capture a thought, source, or observation from anywhere in the app. Threadnote routes it into a thread or leaves it lightly unfiled when confidence is low. When users reopen a thread, they see the current judgment, open questions, recent movement, and a direct place to continue. Lists collect resources by view, not by problem state. AI stays in the background to classify, connect, and summarize.

### Customer quote
"Most apps help me save notes. Threadnote helps me resume the actual problem." said an independent researcher using the early prototype.

### How to get started
Open Threadnote on Mac, create or reopen a thread, and start with one capture.

## FAQ

### Internal FAQs

**Q: Why is this a new phase instead of another MVP iteration?**  
A: Because the current project shape is still a demo: SwiftPM executable, ad-hoc input handling, lightweight persistence, and UI decisions made before the app shell and AI role were fully defined. The next milestone changes the foundation, not just surface behavior.

**Q: What is the core objective of this phase?**  
A: Turn the prototype into a credible native Mac app foundation: reliable capture/editor, formal app architecture, clarified object boundaries, and a constrained AI role that improves structure rather than writes for the user.

**Q: Why not keep iterating on the current SwiftUI-only prototype?**  
A: The current friction is concentrated in editor, focus, windowing, and workspace behavior. Those are native app problems. SwiftUI can remain part of the UI stack, but the app needs stronger AppKit-backed infrastructure for editor and window behavior.

**Q: What role does AI play in this phase?**  
A: AI is a structure assistant. It classifies captures, suggests thread routing, infers discourse relations, and generates resume context. It is not a default ghostwriter.

**Q: What is intentionally out of scope?**  
A: Real-time collaboration, generic database-style inventory management, broad agent automation, heavy graph UIs, and AI-first writing flows.

**Q: Why move the design language toward Craft?**  
A: Because Craft demonstrates low-noise visual hierarchy, clear sidebar/canvas separation, and document-first calm. We want that clarity, not its full product model.

### Customer FAQs

**Q: Is Threadnote a note app or a writing app?**  
A: It starts as a note and source capture app, but its real job is to restore the state of a problem so you can continue thinking or writing.

**Q: Can I use it like Notion to manage a giant reading database?**  
A: Not as the primary job. Threadnote supports Lists as resource views, but Threads remain the core unit for ongoing thinking.

**Q: Will AI rewrite my notes?**  
A: No. AI may classify, connect, and summarize. The author remains in control of the actual argument and output.

**Q: Can I collect books, articles, and files?**  
A: Yes, as sources and list resources. But they remain in service of a thread or a resource view, not a generic inventory system.

**Q: What devices are in scope first?**  
A: macOS first. This phase is about making the Mac app solid enough to use daily.
