# PR/FAQ Draft: AI Intake Architecture

## Press Release

### Headline
Threadnote turns raw input into organized thread work without asking users to file, tag, or structure notes first.

### Subtitle
This phase rebuilds Threadnote's AI around input understanding first: item type, objects, thread fit, thread state, and prepare output all become explicit product responsibilities instead of side effects of retrieval.

### Date
March 13, 2026

### Intro paragraph
Today we are starting the AI Intake Architecture phase for Threadnote. This phase sharpens the product around its actual promise: the user should only have to type. Threadnote should take responsibility for understanding what the note is, what it refers to, where it belongs, what the thread currently means, and how to turn that thread into useful output.

### Problem paragraph
Threadnote already has useful retrieval and memory infrastructure, but those layers are currently doing too much front-line intake work. That means the system can find old matching material, but it does not yet consistently understand a new note on its own. The result is backward-looking automation: retrieval can help classify, but input understanding, object extraction, candidate claims, and thread routing do not yet exist as first-class product behaviors.

### Solution paragraph
AI Intake Architecture corrects that boundary. Capture-time intelligence becomes its own layer. Every new note will first be interpreted into a normalized item: its likely type, mentioned objects, candidate claims, and routing signals. Thread routing then uses those signals together with a compact thread signature. Retrieval and memory stay in the system, but they move back to their proper role: supporting thread-state synthesis and prepare output instead of acting as the main intake brain.

### How the product works
When a user types a note, Threadnote first interprets the note itself. It infers the item type when possible, extracts people, organizations, places, products, or other objects, derives candidate claims, and decides whether the note belongs to an existing thread. If confidence is high, the note is routed automatically; if not, it stays in the inbox with suggestions. After notes land in a thread, Threadnote synthesizes the thread's current state into Restart Note, current judgment, open loops, and next action. When the user opens Prepare, Threadnote converts the thread into a focused writing context instead of a full archive dump.

## FAQ

### Internal FAQs

**Q: Why is this a new phase instead of a follow-up task inside AI memory / RAG?**  
A: Because this is not a retrieval-quality tweak. It is a product-boundary correction. The system needs an explicit intake layer, not more routing heuristics on top of retrieval.

**Q: What is the core objective of this phase?**  
A: Make "user only inputs, system organizes" true at the intake boundary, not only at the retrieval boundary.

**Q: What is deliberately out of scope?**  
A: Full autonomous writing, rewriting historical notes, bulk thread creation, high-confidence automatic decisions on behalf of the user, and broad multi-thread agent workflows.

**Q: What are the main system layers after this phase?**  
A: Capture Intelligence, Thread Signature Routing, Thread State Synthesizer, and Prepare Composer. Retrieval and memory remain supporting infrastructure.

**Q: What does success look like?**  
A: A user can type a raw note without explicit structure and still get a reasonable item type, useful object extraction, a trustworthy route decision or suggestion, and a coherent thread restart state.

### Customer FAQs

**Q: Do I still need to write `#claim` or `@objects` manually?**  
A: You still can, but the system should no longer depend on that to do basic organization well.

**Q: Will Threadnote rewrite or move my old notes without asking?**  
A: No. This phase improves interpretation and routing for new input, but it does not rewrite history.

**Q: What happens when Threadnote is not sure where a note belongs?**  
A: It should keep the note in inbox and show suggestions, instead of forcing a confident-looking but unreliable route.

**Q: Does this require a cloud model?**  
A: No. Deterministic fallbacks remain required. Cloud backends may improve interpretation quality, but the product cannot depend on them for core operation.

**Q: Will this create lots of threads automatically?**  
A: No. Automatic thread creation remains out of scope for this phase.
