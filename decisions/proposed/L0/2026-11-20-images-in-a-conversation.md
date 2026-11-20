# Decision: Images in a conversation

Status: proposed

## Problem

A conversation carries text only. The person cannot send a screenshot or
a picture, the chat cannot show one, and a tool that meets an image (a
telega buffer, a PNG in the project) can only print it as text: the
read of a telega buffer printed its thumbnails as base64 and filled the
result. The models can take images: `deepseek-flash` lists
`"input": ["text","image"]` with resize limits (2000×2000, 4.5 MB), and
the bridge already builds `image_url` parts for `openai-completions`.

## Proposal

- **Storage (L0, L1).** An image is copied once into
  `superchat-directory/images/` under its SHA-256 and extension. An
  entry names it in `:images`, a list of `(:file NAME :mime TYPE)`. The
  Tape holds no image bytes and no base64.
- **Sending (L5).** `C-c C-a` attaches a file, and yanking an image from
  the clipboard (macOS `pngpaste` or Emacs's `yank-media`) attaches it.
  The input shows each attached image as a small thumbnail; sending puts
  them on the `user` entry.
- **Requests (L2).** `superchat-llm-request` takes `:images` on a user or
  tool message and passes them to the bridge, which reads, resizes to
  the model's `inputLimits` and encodes them. A model whose `input` lacks
  `image` gets the text `[image NAME left out: this model takes no
  images]` in its place, and the chat says so once.
- **Tools (L2).** `read` of an image file returns the image, not its
  bytes. `eval_elisp` prints an image object or a string carrying a
  `display` image as `[image]`, never its data.
- **Display (L5).** An entry's images show inline in the chat, scaled to
  the window's width, through `insert-image`; in a terminal Emacs, as
  `[image NAME]` with a link that opens the file.

## Alternatives considered

- Keep images as blobs in the Tape's SQLite. Not taken: views and
  `tape_query` read rows as text, and one blob would weigh every query
  that touches the entry.
- Keep only the original path. Not taken: the file can move or vanish,
  and the conversation would lose what was said about it.
- Send base64 inside the text of a message. Not taken: it is the noise
  this note starts from, and models read it as text.

## Acceptance criteria

- A PNG attached with `C-c C-a` reaches `deepseek-flash` as an
  `image_url` part, and the answer describes it.
- The Tape row of that entry holds the file name, no image data.
- The same conversation with `/model deepseek-v4-pro` sends the
  left-out text, and the request succeeds.
- `read` of a PNG and the earlier telega `eval_elisp` query return no
  base64.
- The chat shows the image inline in a GUI Emacs and a link in `-nw`.

## Risks

- Every image is sent again with every later request of the
  conversation; a long one with screenshots grows costly. A budget
  rule like the one for text entries may be needed.
- Clipboard images depend on the platform; `yank-media` needs Emacs 29+
  and a handler for `image/png`.
- `superchat-chat.el` is at 863/900 lines; the attach and display code
  may not fit there, and that must be raised with the person first.
