# Working in your own buffers

These commands work where you are, in any buffer; the chat window does not need to be open. The captures below are the buffer text Emacs shows, taken from a batch run with a scripted model; "…" marks a shortened path.

## Inline ask

`M-x superchat-inline-ask` asks about the code at point. The answer appears in an overlay under the line, and the file is never edited.

```text
def total(prices, tax):superchat inline · Ready · M-RET menu
It returns the sum of the prices with the tax added; tax is a rate such as 0.08.

    s = 0
    for p in prices:
```

`C-c SPC` in the prompt cycles what is sent as context (function, sexp, line, window, buffer, none). On the overlay: `r` reply, `v` open the conversation, `w` copy, `q` clear, `M-RET` menu.

## Inline act

`M-x superchat-inline-act` has the AI do a task right where you are. It can read other buffers and edit this one or others; the chat window does not open. Progress and the answer appear in an overlay under the line. At the end the overlay says how many tool calls the Run made and which buffers it left edited and unsaved; saving is yours.

```text
def total(prices, tax):superchat inline · Ready · M-RET menu
Replaced the loop with sum(prices).

— 1 tool call
1 buffer edited by this Run is unsaved -- saving is yours; /changes to review:
- main.py -> /…/main.py

    s = sum(prices)
    return s * (1 + tax)
```

When a tool needs approval, the question is published and Emacs goes on until you answer it (`M-x superchat-approval-allow` / `superchat-approval-deny`, or `y` / `n` in `M-x superchat-pending`), as in the chat window. `C-c SPC` in the prompt cycles the context, as for inline ask. On the overlay: `d` diff of an edited buffer against its file, `r` follow-up with tools still available, `v` open the conversation, `w` copy, `q` cancel and clear, `M-RET` menu. The review screen for the unsaved buffer is described in [permissions](permissions.md#your-edits-stay-in-the-buffer).

## Rewrite a region

`M-x superchat-rewrite-region` rewrites only the selected text. The text around it (its Org or Markdown heading section, or the function it is in) goes along as a read-only reference. The result streams into a preview over the region; your file stays byte-for-byte unchanged until you accept.

```text
* Monday
REWRITE  Ready  C-c C-a accept  C-c C-k reject  C-c C-d diff  RET menu
The meeting has moved to Thursday at 3 pm. Please bring the draft budget.
```

`C-c C-a` accepts (one undo step), `C-c C-k` rejects, `C-c C-c` cancels, `C-c C-r` refines, `C-c C-d` shows a diff, `C-c C-m` merges as conflict markers, `C-c C-n` / `C-c C-p` move between previews, `RET` opens the menu. A rectangle region gets one preview per segment. An answer that brings back the request's markers is refused, and one much longer than the selection is flagged. `superchat-rewrite-confirmation` is `overlay` (the preview, the default) or `auto`.

## Annotate

`M-x superchat-annotate` hangs remarks on the region, or on the whole buffer, each on the text it is about. The piece is underlined and the remark shows as a block under its line; your text is never changed.

```text
;;; init.el
(use-package corfu
  :ensure t
  └─▶  use-package-always-ensure is already t here, so this line 
       repeats it. 
  :custom
  (corfu-auto-delay 1.0))
  └─▶  A delay of 1.0 second makes completion feel slow; 0.2 is common. 
```

`superchat-annotate-remark-style` set to `eol` puts a remark at the end of the line instead of `below` (the default). A remark is placed only where the model's quote is found on the line it named or within two lines; the rest are counted and listed, not placed. `M-n` / `M-p` on a remark move between them, `M-x superchat-annotate-list` lists them, `M-x superchat-annotate-clear` removes them. It works best on configuration and notes; less so on code, and it is not dependable on a small local model there.

In a conversation, a message that holds `>annotate` is answered with the conversation's history and without tools, and its answer is also hung on the buffer the conversation was called from; `>annotate NAME` or `>annotate "NAME"` names another buffer or a file. It applies to that one message.

## Notes

`M-x superchat-note` writes your own note on the region (else the symbol or line at point) in any buffer, a file or a diff; the text is not changed. On a note, change its words; empty words drop it. Point on a note shows it. `M-x superchat-notes` lists every note; `M-x superchat-note-attach` puts all of them on the message you are writing, above `* User:`, numbered, each with its file, line and quoted text; once sent they are done. Notes on files are kept and come back when you visit the file.

```elisp
(keymap-global-set "C-c n" superchat-note-map)
```

In `superchat-note-map`: `n` note, `d` drop the note at point (with a region, every note in it), `D` drop every note in the buffer after asking, `l` list, `a` attach, `]` / `[` next and previous.

## Which conversation these join

Inline ask, inline act and region rewrite go into the conversation of where you are: from a file inside a project, that workspace's main conversation (a chat window beside the file that shows another conversation is switched to it); from any other buffer, that buffer's own conversation in Home (see [the chat](chat.md#which-conversation-opens)); from a chat window, the conversation it shows. Each request carries only what it points at and your words, not the conversation's earlier turns, like Cursor's inline edit and gptel's rewrite. A follow-up in the same overlay (`r`) carries that overlay's own first exchange, and whether you saved or discarded what it edited since. The turn is recorded in the conversation: the chat window shows it when it ends, with a button back to the file and line, and later messages in the chat carry it. While the chat is working, an in-place request waits its turn. To bring the conversation's history into a question about some code, send the code to the chat with `M-x superchat-send-region` and ask there.

## Org Babel and Magit

An Org source block runs as a question; the answer lands in `#+RESULTS:`.

```org
#+begin_src superchat
explain what a zettelkasten is in one sentence
#+end_src
```

With Magit loaded, `M-x superchat-magit-commit-message` writes a commit message from the staged diff: in a commit message buffer it inserts the message at point, elsewhere it shows it in the echo area. `superchat-magit-commit-style` is `conventional` (the default) or `freeform`.
