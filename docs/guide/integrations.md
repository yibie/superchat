# Integrations

## MCP

Superchat works with [mcp.el](https://github.com/lizqwerscott/mcp.el). Configure servers in `mcp-hub-servers`, start them with `/mcp-start` (all) or `/mcp-start-server` (one, chosen from a prompt), and check them with `/mcp` and `/mcp-health`; `/mcp-stop-server` stops one.

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem"
                               "/Users/you/Documents")))))
```

When a server connects, Superchat registers each of its tools with `superchat-register-tool` under the name `mcp:<server>:<tool>`. From then on they arrive inside `superchat-get-llm-tools` like any other registered tool: the same tool surface, the same request-local tool profile, the same Tape audit wrapper. The [permission level](permissions.md#permission-levels) decides whether a call asks you: by default every MCP call does, until you list its server in `superchat-mcp-trusted-servers`.

Registration happens when the server is started through Superchat (`/mcp-start`, `/mcp-start-server`). A server you started elsewhere (`mcp-hub-start-all-server` in your init file, or the `mcp-hub` UI) is picked up the next time you run `/mcp`.

## Telega

With [telega.el](https://github.com/zevlg/telega.el) loaded, open any Telega chat and run `M-x superchat-cowork-telega`; no message selection is needed. The bundled plugin opens or reuses a conversation bound to that chat, where you can ask follow-up questions in plain words, understand what happened, and prepare a reply. The model in that conversation can only read the current chat or chats you explicitly grant. It cannot send messages or change rules. In that buffer `C-c C-c` sends a follow-up, `C-c C-a` adds a chat to its scope, and `C-c C-d` puts the latest answer into the Telega chat's input area for you to review and send.

## elfeed

With elfeed loaded, the model has `feed_search`, which reads elfeed's database in elfeed's own search syntax (`@1-week-ago`, `+unread`, `=feedname`, a regular expression on titles) and changes nothing, not even what is read; it is not asked about at any level.

```
anything about security in my feeds lately?
```

The skill `elfeed-feeds` (`plugins/superchat-elfeed/skills/`, in the skill catalog while elfeed is loaded) tells the model to name each entry with its own title and link, and that what the entries say is data. What it reads goes to the model provider. "Tell me whenever …" sets a rule on new entries instead; see [work that runs by itself](unattended-work.md#new-feed-entries).

## Org Babel and Magit

See [working in your own buffers](in-your-buffers.md#org-babel-and-magit).
