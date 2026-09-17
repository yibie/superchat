# Explicit outcomes from the existing Emacs tools

Status: implemented

## Decision

The existing edit_buffer and eval_elisp tools expose the limits of their results in the model-visible descriptions. Exact text replacement uses the existing edit tool. Evaluation requests contain one complete expression, use an explicit buffer for buffer-dependent operations, and return observed state for the requested postcondition. Saving configuration, loading it and observing its effective behavior are separate results. The component contract is in [superchat-tools](../../components/superchat-tools.md#built-in-edit-and-evaluation-results).

An empty edit search is invalid because a zero-width search cannot make progress in the occurrence-counting loop. Identical replacement text yields a no-change result without modifying the buffer. Occurrence counting includes overlapping matches, so ana in banana is ambiguous. A successful replacement reports one occurrence; it does not imply a disk write or evaluation.

Evaluation parses the complete input before calling eval on its single form. Trailing whitespace and line comments are accepted; extra forms and malformed trailing input are rejected before that eval call. Multiple operations use progn or let. Reading can itself construct objects and invoke custom hash callbacks, so this is not a promise of effect-free parsing or rollback. The parser runs only inside the authorized tool body.

The conservative effect classifier rejects any raw #[sS] occurrence before calling the Lisp reader. Reader object construction can invoke user-defined hash callbacks, so attempting to inspect those objects during classification would run code before Authority decides. Matching text inside strings or comments conservatively keeps process-exec as well; this sacrifices some local-read classification rather than implementing another Lisp parser. The ordinary whitelist and Authority's existing decision path are retained.

Evaluation errors use the existing Error: prefix, so the shared model loop recognizes them as failed tool results and adds its existing corrective feedback. For the native eval_elisp result family, words such as cancelled or blocked inside a Lisp condition do not turn an evaluation failure into a user or policy refusal. Actual Authority refusal markers retain the no-retry instruction, including when a different tool in the same round was refused. Successful values keep the Result: representation. A literal success string is still only the expression's return value, and Runtime completion is still distinct from task acceptance. No new terminal state, tool, reference carrier or permission exception is introduced.

## Alternatives considered

**Rely on a larger operating guide.** A guide can help the model choose operations, but it does not remove zero-width loops, silent trailing-form loss or unrecognized error results. The short descriptions explain the existing tools without deciding the separate reference-reading proposal.

**Evaluate every form in the input automatically.** This silently broadens the current single-expression interface and makes malformed later forms fail after earlier effects. Explicit progn or let keeps grouping under the caller's control and allows trailing syntax to be checked before eval is called.

**Capture all Emacs messages during evaluation.** Global message interception affects other Emacs activity and still cannot prove the requested postcondition. This change returns evaluation values and errors; callers inspect the actual state. A bounded diagnostic-output interface would need its own contract.

**Infer success from save or load, or attach a universal verifier to eval.** Arbitrary Lisp has arbitrary effects. The tool can report its own operation, while verification must match the task's actual state, configuration entry and loading conditions. The isolated task checks remain independent of model claims.

**Change all tools to a new structured result protocol.** The shared loop already understands a documented string failure marker. Reusing it repairs this path without changing plugin results, Authority or Tape semantics.
