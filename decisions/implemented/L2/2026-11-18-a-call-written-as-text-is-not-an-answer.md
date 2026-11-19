# Decision: A call written as text is not an answer

Status: implemented

## Problem

Asked for a response with no tool definitions in the request, DeepSeek wrote
its own call markup as ordinary text: `<｜｜DSML｜｜ calls>`, then an
`invoke` naming a tool, straight into what the person reads. Nothing in
`spec/L2-capabilities.md` said what a bridge does with call syntax that
arrives as text, so `superchat-llm.el` passed it on unchanged. `NOTES.md`
recorded it.

The bridge cannot help here: by the time the text reaches this module it is
one `text` content block among the others, indistinguishable from a sentence
by anything the provider sends. The markup's own characters are the only
evidence there is.

## Decision

Where the markup begins, the answer ends. The text of a response is cut at
the first `<｜｜`, trailing whitespace removed, and what is left is delivered
as the answer. A response left with nothing is not an answer but a failure:
ON-ERROR, kind `other`, the message `The model wrote a tool call as text
instead of calling one`.

The rule sits beside the one that already refuses an empty response, and for
the same reason: an answer is something the person can read. Half a sentence
there is worth keeping — it is what the model was saying before it went wrong
— but the markup itself is never worth showing, and a run that gets a
failure can be told about it, which a text full of delimiters cannot be.

## Alternatives considered

- Keep the tools in the request always, so the model never has a reason to
  write a call as text. That is the right answer where a run reaches this
  situation (L3, the round limit, which now offers its tools and says to call
  none). It cannot be this module's: `superchat-llm-request` is given the
  tools, and a caller with nothing to offer has a real reason to offer
  nothing.
- Show the text and let the run notice. The text has no call in `:calls`, so
  the run cannot tell it from an answer; the person reads delimiters and has
  to work out what happened.
- Cut at the whole `<｜｜DSML｜｜ calls>` and keep what follows. What follows
  is the delimiters of the calls themselves, and there is no knowing where a
  provider's markup ends.
- Deliver the cut text as the answer and say nothing about the cut. Then a
  provider that writes only markup gives an empty answer, and the run reports
  it as `empty`, which is true but says the wrong thing: nothing was empty,
  a call was written where a sentence belonged.

## Consequences

- `spec/L2-capabilities.md` says the rule and names DeepSeek's markup. Its
  ceiling was 3800 words and the file stood at 3794; it is 3990 words now,
  five percent more, for the reason the interface and agent ceilings give.
- A response with text before the markup answers with that text; the run gets
  no failure and the tool it wanted is not run. The next request is the
  run's, as with any answer.
- `test/test-llm.el` holds both cases. Replacing the markup constant with
  something no response contains fails both.
