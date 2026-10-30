# DeepSeek 中途断开时，curl 报什么、superchat 重不重试

superchat 真实使用时，请求经 llm.el、plz 和 curl 发给 DeepSeek，走的是 HTTP/2。同样是一次中途断开，在 HTTP/2 上 curl 报 92、16、18 或 56。测量时 superchat 的自动重试只认其中的 56，2026-10-29 起四个都认（[重试决定的第三次修订](../decisions/accepted/2026-09-23-retry-transient-provider-errors.md)）。ARC 测试机看到的断开发生在 HTTP/1.1 上（代理用 Python 的 `http.client`），不能直接套用到真实使用。2026-10-29 测量。

## 做法

- 三个 curl：`/opt/local/bin/curl` 8.10.1（OpenSSL 3.3.2，PATH 里排第一，ARC 测试机里的 superchat 用的就是它）、`/usr/bin/curl` 8.7.1（LibreSSL 3.3.6）、`/opt/homebrew/opt/curl/bin/curl` 8.22.0（OpenSSL 3.6.4）。
- llm.el 0.32.1、plz 0.9.1，都取自测试用的 ELPA。plz 不指定协议，用 curl 的默认值：服务端提供 HTTP/2 时就用 HTTP/2。三个 curl 访问 `api.deepseek.com` 都协商成 HTTP/2。
- 本地模拟：`.scratch/arc-bench/tools/drop-emulator.py` 起一个本地 TLS 服务器（HTTP/2 和 HTTP/1.1），在回复头之前或之后按各种方式断开，三个 curl 都用 plz 给 curl 的参数去请求。结果在 `.scratch/arc-bench/runs/drop-emulator-20261029-090627/results.json`。下表记的是 curl 的退出码；curl 自己的 `%{errormsg}` 在两个 16 的情况里和退出码对不上，所以不用它。
- 经过 llm.el 和 plz 的端到端：`.scratch/arc-bench/tools/drop-emacs-check.el` 把 `make-llm-deepseek` 的 URL 指向本地服务器，记下错误回调收到什么，再交给 `superchat--llm-transient-error-p` 判断。
- 真实抓取：`.scratch/arc-bench/tools/drop-capture.py` 不经代理，用 plz 给 curl 的参数，直接向 DeepSeek 重放 ARC 25 个游戏那一批录下的请求，等它断开一次。结果在 `.scratch/arc-bench/runs/drop-capture-20261029-090433/`。

## HTTP/2 上的断开，curl 的退出码

三个 curl 结果相同，不同的地方另外注明。

| 断开方式 | 在回复头之前 | 在回复头之后 |
|---|---|---|
| RST_STREAM，错误码 INTERNAL_ERROR 或 CANCEL | 92 | 92 |
| RST_STREAM，错误码 NO_ERROR | 92 | 0，正文不完整 |
| RST_STREAM，错误码 REFUSED_STREAM | 56 | 56 |
| GOAWAY 后关闭连接 | 16 | 18 |
| 先发 TLS close_notify，再 FIN | 16（8.22.0 是 56） | 18 |
| 不发 close_notify，直接 FIN | 56 | 56 |
| TCP RST | 56 | 56 |

HTTP/1.1（`--http1.1`）作对照：回复头之前，先 close_notify 再 FIN 是 52，直接 FIN 是 56（LibreSSL 的 curl 是 52），TCP RST 是 56；回复头之后（chunked），先 close_notify 再 FIN 是 18，直接 FIN 是 56（LibreSSL 的 curl 是 18），TCP RST 是 56。Python 的 `http.client` 对回复头之后的两种 FIN 都报 `IncompleteRead(0 bytes read)`，和 ARC 测试机代理记下的一样；对 TCP RST 报 `ConnectionResetError`，代理一次也没记到。所以测试机看到的断开是 FIN，不是 RST。

## superchat 收到什么

经过 llm.el 0.32.1 和 plz 0.9.1（用 8.10.1 和 8.7.1 两个 curl，结果相同）：

| curl 退出码 | 错误回调收到的（类型都是 `error`） | 自动重试（测量时） | 2026-10-29 起 |
|---|---|---|---|
| 92 | `Unexpected curl process status:"exited abnormally with code 92\n" code:92.  Please report this bug to the `plz' maintainer.`（`\n` 在消息里是一个真的换行） | 不重试，执行出错结束 | 30 秒后重发 |
| 16 | `curl error code 16: nil` | 不重试 | 30 秒后重发 |
| 18 | `curl error code 18: Partial file. Only a part of the file was transferred.` | 不重试 | 30 秒后重发 |
| 56 | `curl error code 56: Failure in receiving network data.` | 30 秒后重发 | 不变 |
| 0，正文不完整 | 没有回调：plz 解析正文时在自己的计时器里报 `json-end-of-file`（`Error running timer 'plz--respond'`），错误没有传出来 | 只能等无响应窗口到点 | 2026-10-30 起报 `Unparsed response body: (json-end-of-file …)`，30 秒后重发 |

改动后用三个 curl 经 llm.el 和 plz 重跑了 HTTP/2 的 16 种断开（`.scratch/arc-bench/runs/drop-retry-check-20261029-183252/emacs-check-joined.txt`）：48 次里 45 次判为重试，剩下 3 次都是回复头之后的 RST_STREAM NO_ERROR，没有回调。8.22.0 的"先 close_notify 再 FIN、回复头之前"这次报 16，前面直接用 curl 测时报 56，两个都重试。

- plz 0.9.1 只把 1 到 90 的退出码当作 curl 错误处理（`plz.el:890`）；92 走 `plz.el:900` 的分支，变成上表那句话。`plz-curl-errors` 里没有 16 和 92 的说明，所以是 `nil`。
- `superchat--llm-transient-error-p`（`superchat-llm.el:271`）测量时只认 HTTP 429/5xx、`Operation timeout.` 和 `curl error code` 6、7、35、52、55、56；现在多认 16、18、92，和 plz 对 92 的那句话。重试前还要求这次尝试没有收到文字和推理（`superchat-llm.el:3683`、`:3684`），不看工具调用有没有收到一部分；[无响应重发](../decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md)看。不看也安全：llm.el 要等整段回复收完、解析以后才执行工具（`llm-provider-utils.el:477-519`），流式时收到的半截工具调用不会执行，superchat 也不显示它，重发不会重复任何事。
- 退出码 0、正文不完整这一种，和 ARC 测试机代理造成的情况是同一类：错误在 plz 的计时器里抛出，没有交给 llm.el 的错误回调。[事后分析 0001](../postmortems/0001-a-broken-tool-call-lost-its-type.md) 是同一条路上的另一种表现。

## DeepSeek 在 HTTP/2 上什么时候发回复头

真实抓取的 1000 次请求里，998 次的第一个字节离请求结束不到 1 秒（结束时间减第一个字节的时间：中位数 0.026 秒，最大 6.98 秒）。DeepSeek 在 HTTP/2 上要等回复生成好才发回复头，中间没有 HTTP/1.1 上那种隔一阵发一个换行的保活。所以 HTTP/2 上的中途断开，多半落在上表"回复头之前"那一列：92、16 或 56。

## 真实抓取

- 1000 次请求，8 个并发，北京时间 2026-10-30 00:04 到 00:23。`/opt/local/bin/curl` 524 次、`/usr/bin/curl` 196 次、8.22.0 的 curl 280 次。全部退出码 0、HTTP 200、HTTP/2，一次断开也没有。花费 $1.07。
- 这不说明高峰时也不会断。ARC 25 个游戏那一批遇到 9 次断开（7 次在 200 之后，2 次在回复之前），都在北京时间 17:50 到 23:20；那批 superchat 请求的中位数是 9.7 秒，这次是 4.9 秒。
- DeepSeek 在 HTTP/2 上会不会中途断开、用哪种方式断开，没有测到。测试机见到的是 HTTP/1.1 上保活换行之后的 FIN；HTTP/2 上没有保活阶段，这种断开在 HTTP/2 上有没有对应的形式，不知道。

## 候选

1. 已做（2026-10-29，用户"同意"）：自动重试多认 16、18、92，也认 plz 0.9.1 对 92 的那句 `code:92`，沿用"没收到文字和推理才重试"。见[重试决定的第三次修订](../decisions/accepted/2026-09-23-retry-transient-provider-errors.md)。

2026-10-30 用户说"继续断开研究里的 2 到 4"：

2. 高峰时段再抓一次：北京时间 2026-10-30 20:53 到 21:36（原定 18:00 那次因脚本错误一个请求都没发），4 个并发。`/opt/local/bin/curl` 780 次、8.22.0 的 curl 134 次、`/usr/bin/curl` 86 次。1000 次全部退出码 0、HTTP 200、HTTP/2，一次断开也没有。花费 $1.23。请求用时中位数 4.6 秒，95% 在 33.7 秒内，最长 84 秒；991 次的第一个字节离请求结束不到 1 秒。结果在 `.scratch/arc-bench/runs/drop-capture-peak-20261030-2055/`。两次抓取合计 2000 次没有断开，DeepSeek 在 HTTP/2 上怎么断开仍然不知道；只覆盖了高峰时段里的 43 分钟。不再继续抓。
3. 已做：在 superchat 一侧接住，见[重试决定的第四次修订](../decisions/accepted/2026-09-23-retry-transient-provider-errors.md)。改动后用本地服务器、三个 curl 重跑"回复头之后 RST_STREAM NO_ERROR"，三次都报 `Unparsed response body: (json-end-of-file 2 nil 1)` 并判为重试（`.scratch/arc-bench/runs/drop-unparsed-check-*/emacs-check.txt`）。
4. 只写了草稿，没有发：plz 的 #70（"Exit code 92 is not handled"）2024 年已关闭，维护者说有更多信息再改，所以写的是在 #70 下的评论；plz-media-type 没找到有人报过，写的是新 issue。在 `.scratch/issues/2026-10-30-upstream-plz-exit-code-92.md` 和 `…-plz-media-type-parse-error.md`。plz-media-type 的问题在发布版 0.2.4 上用一个不依赖 superchat 的复现跑过；上游最新版（0.2.5-pre）那段代码相同，没有跑。

流式请求另有一处（2026-10-30 用假 curl 测）：截断落在一个事件中间时，执行以 `[Missing final response from llm]` 失败、不重试；截断落在一个完整事件之后、`[DONE]` 之前时，执行算作完成，回答只有已经收到的那部分，没有任何提示。superchat 只在不带工具时对 DeepSeek 用流式。已修，只管 DeepSeek，见[重试决定的第五次修订](../decisions/accepted/2026-09-23-retry-transient-provider-errors.md)。

用户自己的 Emacs 用哪个 curl：`~/.emacs.d` 里没有 `plz-curl-program` 和 `exec-path-from-shell`；`~/.config/nova-emacs` 用 `exec-path-from-shell` 导入登录 shell 的 PATH（`modules/nova-core.el:44-48`），按这个 PATH 找到的是 `/opt/local/bin/curl` 8.10.1（排在它前面的目录里都没有 curl）。平时用哪一套配置启动，没有核实。上面的表三个 curl 都覆盖了。
