# HTTP 层选型

本包需要从 Emacs 里访问 LLM 服务，最关心六件事：

1. **不阻塞 Emacs**：请求进行时用户仍能编辑、C-g 能中断；
2. **SSE 流式**：`text/event-stream` 必须逐块（chunk）交付，而不是等连接结束；
3. **取消**：用户按停止键要在网络层真正断掉连接；
4. **超时**：连接/总时长都要有上限，且到点真的退出；
5. **代理**：企业网络、`http_proxy`/`https_proxy`/`no_proxy`；
6. **二进制**：图像下载/上传不能破坏字节，且不把 MB 级数据塞进 Emacs 缓冲区。

此外还有一条「错误与 404 的可判定性」：HTTP 非 2xx 必须能与「连接失败」「超时」区分开。

候选三种：`url.el`（内置）、`plz`（基于 curl 的 Emacs 包）、直接用 `make-process` 调 `curl`。

以下所有数字都来自本机实测。基准脚本是 `/tmp/pi-http-bench.el`、`-2`、`-3`、`-4`、`-5`、`-6`
与 `/tmp/pi-http-proxy.el`、`-2`、`-3`；统一用 `emacs -Q --batch` 跑，plz 用
`-L ~/.cache/superchat-test-elpa/plz-0.9.1` 加载。网络探测一律 `make-process` 异步启动 curl，
在后续调用里 `accept-process-output` 读结果，从不阻塞等待。环境：macOS，未设置任何 `*_proxy`。

## 实测数据

通用基准：`anthropic.json`（约 12 KB，`https://unpkg.com/@earendil-works/pi-ai@1.4.0/dist/providers/data/anthropic.json`）。
`ticks` 是 10ms 定时器在请求期间的触发次数，用来看 **Emacs 的空闲程度**；它只说明定时器有没有机会跑，
是否阻塞要看调用者有没有拿回控制权。

### 1. 不阻塞 / 耗时

```
url.el sync                          1.562s ticks= 145 bytes=12632
url.el url-retrieve async            0.290s ticks=  28 bytes=12632
plz sync                             1.032s ticks= 103 bytes=11820  (code=200)
plz async (:then, 立即返回)          1.085s ticks=   0  (调用者持续运行到回调)
curl async (make-process)            1.077s ticks= 107 bytes=11920
make-process 返回控制权耗时          0.0037s（进程继续在后台跑）
```

结论：`make-process` 约 4ms 内返回，进程后台运行；`plz` 的 `:then` 版本同样立即返回；
`url-retrieve` 也是异步，只是把输出塞进隐藏缓冲区再在回调里处理。
三者的同步版本（`url-retrieve-synchronously`、`plz ... :then 'sync`）在请求期间占住 Emacs。

### 2. SSE 增量交付（`https://stream.wikimedia.org/v2/stream/recentchange`，5s 窗口）

```
plz 'get :as 'response                 单回调，5.0s 后 0 字节（无增量交付）
plz 'get :as 'buffer :filter           128 次回调，217 885 字节 / 5.01s
plz :filter 且消费方每块忙 0.05s        93 次回调，125 636 字节 / 3.31s（不丢数据，只是缓冲）
url.el async + 自设 filter             164 次 filter 调用，176 889 字节 / 5.02s
curl -sSN -> process filter            143 次 filter 调用，214 374 字节 / 5.03s，exit=28(--max-time)
url.el 默认（不设 filter）             单回调，无增量
```

结论：**增量交付不是「用 plz 就有」**。

- `plz` 默认把响应体聚合成一个 `plz-response`，对 SSE 不可用；
- 必须传 `:filter (PROC STRING)` 才拿到逐块数据，而该 filter 取代 plz 自己的过滤器，
  用户要自己往进程缓冲区里插数据；
- `url.el` 同样要自己 `set-process-filter`，默认行为是「累积到结束再回调」。

### 3. 取消

```
curl: 启动后 0.2s 内 delete-process    status=signal，进程消失，连接立即断开
curl: 消费方从未等待就取消              status=signal，进程消失
plz 0.9.1: 无 plz-cancel，只能对返回的 curl 进程 delete-process
url.el: url-retrieve 返回缓冲区，需 (delete-process (get-buffer-process buf))
```

结论：三者都能在 Emacs 侧断开，但只有 curl 方案能用 `--connect-timeout`/`--max-time`
在服务端也封顶；`url.el` 的取消是杀底层进程，状态机不清理，异步回调可能收到非 `nil` 的 status；
plz 0.9.1 没有公开的取消 API。

### 4. 超时

```
curl --max-time 2 对无限 SSE：2.04s 结束，exit=28，
  stderr="curl: (28) Operation timed out after 2003 milliseconds with 20854 bytes received"
```

结论：`--max-time` 是**总时长**上限，到点终止，退出码 28 可判定。
`plz` 的 `:timeout` 也映射到 curl，但对无限流要在 `:else` 里解包 `plz-error`/`plz-curl-error`。
`url.el` 只有 `url-retrieve-synchronously` 有超时参数；`url-retrieve` 无内置总超时，
要自己 `run-at-time` + `delete-process`。

### 5. 错误与 404

```
curl: 404 -> exit=0，正文是 "404"；状态码需 -w '%{http_code}' 取，或 -f 得 exit=22
plz:  非 2xx -> :else 收到 plz-error（plz-http-error / plz-curl-error），带 status 与 body
url.el: 非 2xx 不报错，回调 status 为 plist，需自己解析首行
```

结论：plz 的错误模型最省事（结构化 `plz-error` 内含 `plz-response`）；curl 要约定
`-f` 或 `-w`；`url.el` 的错误分发最弱，异步下拿到状态的时机还不确定。

### 6. 代理

```
环境 https_proxy=127.0.0.1:9（无可用代理）时：
  curl  -> exit=7，"Failed to connect to 127.0.0.1 port 9"，即 curl 读到了环境变量
  plz   -> (plz-curl-error ... (7 . "Failed to connect to host."))，同样走环境变量
  url.el -> 与 http_proxy 无关；只认 url-proxy-services。
             干净环境里设 (("http" . "127.0.0.1:9")) 后请求 https 目标仍是 200，
             即该键对 https 目标不生效，必须按 scheme 另设 https/no_proxy
curl -x http://127.0.0.1:9 显式代理：exit=7，可控
```

结论：curl/plz 继承 `http_proxy`/`https_proxy`/`no_proxy`，与用户 shell 一致——这在 Emacs 里很关键
（Emacs 进程的环境通常来自启动它的 shell）。`url.el` 要单独维护 `url-proxy-services`，
语义与 curl 不同，实测对 https 的行为还不符合直觉。

### 7. 二进制（图像）

```
b-img = PNG, 224 566 字节
curl -sS -o TMPFILE（:buffer nil）          exit=0，size=224566，首 4 字节 89PNG，ticks=167 / 1.67s
plz :as 'file TMPFILE（同步）               返回文件名，size=224566，1.54s
curl -sS 写进 Emacs 缓冲区(:coding binary)  exit=23，只有 103 字节
plz 'get :as 'response（聚合到内存）        216 210 字节（解码后长度变化），content-type=image/png
```

结论：**二进制必须落盘**。curl 用 `-o FILE` + `:buffer nil`，Emacs 不接触字节；
plz 用 `:as 'file` / `(file FILENAME)`。写进进程缓冲区再拼回来会被编码/行尾转换破坏
（实测 `:buffer buf :coding 'binary` 得到 exit=23 与 103 字节垃圾）。
上传同理：`curl --data-binary @FILE`；plz `:body (list 'file FILE) :body-type 'binary`。

### 8. 依赖

| 方案 | 依赖 |
|------|------|
| `url.el` | Emacs 内置；TLS 依赖 gnutls（官方 macOS 构建自带） |
| `plz` | 外部可执行 `curl` + ELPA 包 plz（0.9.1，另有 `rx`/`eieio` 等内置库） |
| `make-process` + curl | 仅需 `curl` 可执行文件，无 Lisp 依赖 |

macOS 自带 `/usr/bin/curl`，主流 Linux 发行版基本都带。plz 的定位正是「把 curl 的健壮性借给
Emacs」，但它仍是需要绑版本的第三方包，内部对缓冲区、编码、媒体类型有自己的假设。

## 三方案对照

| 维度 | url.el | plz | make-process + curl |
|------|--------|-----|---------------------|
| 异步 | `url-retrieve`（回调 + 隐藏缓冲区） | `:then`/`:else`/`:finally`，立即返回 | `make-process`，3.7ms 返回 |
| SSE 增量 | 默认无增量，需自设 filter | 默认无增量，必须传 `:filter` | `:filter` 直接拿 stdout 块 |
| 取消 | 杀底层进程，状态机不清理 | 0.9.1 无公开 API，杀进程 | `delete-process`，`status=signal` |
| 超时 | 仅同步版有 `timeout` 参数 | `:timeout`/`:connect-timeout` 映射 curl | `--max-time`/`--connect-timeout`，exit 28 可判定 |
| 错误/404 | 最弱，status plist 自解析 | 结构化 `plz-error` | 需 `-f`(22) 或 `-w '%{http_code}'` |
| 代理 | 仅 `url-proxy-services`，与 curl 不同 | 继承 curl 环境变量 | 继承环境变量，可用 `-x` 覆盖 |
| 二进制 | 缓冲区拼接，易损 | `:as 'file` 落盘 | `-o FILE` + `:buffer nil`，最干净 |
| 依赖 | 内置 | 外部 curl + 第三方包 | 外部 curl |

## 推荐

**采用 `make-process` + `curl`，由本包的薄封装 `llm-pi-ai-http.el` 对外提供**；`plz` 不作依赖。

理由（按权重）：

1. **SSE 是主路径**。本包 `event-stream` 的接口就是「增量事件」，三种方案里只有 curl 的 stdout
   块能一对一映射到 chunk 边界，实测 5s 内 143 次 filter 稳定交付、不丢数据。plz 要绕过它的
   聚合模型，`url.el` 要自己接管进程过滤器——都是在别人之上再写一层，却换不来优势。
2. **取消与超时是用户可感知的**。`delete-process` + `--max-time` 语义最直白，exit 28 / signal
   都可判定；`url.el` 取消不清理状态机，plz 0.9.1 没有取消接口。
3. **二进制必须绕开 Lisp 字符串**。`-o FILE` 是唯一实测无损的方式，图像下载/上传无需在
   Emacs 里持有百 KB 以上字节。
4. **代理与用户 shell 一致**。curl 继承 `http_proxy`/`https_proxy`/`no_proxy`；`url.el` 要另建一套
   配置，且实测其 https 行为不符合直觉。
5. **依赖最少**。curl 是 macOS/Linux 基本盘；plz 是第三方包，还要绑它的补丁节奏。

代价与实现约束：

- 自己实现：状态行/头解析、4xx/5xx 判定（`-f` 或 `-w`）、curl 可用性检测
  （`executable-find "curl"` 失败时给可读错误 `llm-pi-ai-http-curl-missing`）；
- 流式：`curl -sS -N --max-time N -H 'Accept: text/event-stream'`，`:coding 'utf-8-unix`（二进制用
  `no-conversion`），SSE 解析留在 Lisp 侧（对应上游 `utils/event-stream`）；
- 取消 = `delete-process`（curl 收 SIGTERM 即断开），必要时升级 `kill-process`（SIGKILL）；
- 默认只提供异步入口；同步版本仅给 `sync/` 脚本与测试用，并集中在 `llm-pi-ai-http-sync` 一个函数里，
  便于审计「哪里用同步」。

不作为默认的备选：将来若需要 plz 的媒体类型能力（`application/json-array`、`x-ndjson`），
可作为可选后端接在同一 `llm-pi-ai-http` 接口之后。`url.el` 只适合一次性小 JSON，
本包的流式、取消、二进制三条都要额外绕路，维护成本高于自封一层 curl。

## 附：基准脚本

| 脚本 | 覆盖 |
|------|------|
| `/tmp/pi-http-bench.el` | 同步/异步耗时、SSE 单回调 vs filter、取消 |
| `/tmp/pi-http-bench2.el` | plz `:then` 非阻塞、url-retrieve 非阻塞、curl 写文件、plz 落盘 |
| `/tmp/pi-http-bench3.el` | plz `:then` 耗时、curl `-o` 文件与 PNG magic、早取消、`--max-time` |
| `/tmp/pi-http-bench4.el`、`-5` | plz `:filter` 增量、消费方变慢时的行为、plz 无取消 API |
| `/tmp/pi-http-bench6.el` | url.el 同步/异步/自设 filter 三态对照、curl stdout filter |
| `/tmp/pi-http-proxy.el`、`-2`、`-3` | 代理：环境变量 vs `url-proxy-services` |

复现示例（异步，非阻塞）：

```elisp
(make-process :name "probe" :buffer "*probe*" :noquery t
             :command (list "curl" "-sS" "--max-time" "20"
                            "-o" (expand-file-name "manifest.json" temporary-file-directory)
                            "-w" "%{http_code}"
                            "https://unpkg.com/@earendil-works/pi-ai@1.4.0/dist/providers/data/.manifest.json")
             :coding 'utf-8-unix)
;; 之后的调用里再 accept-process-output 读 *probe*，或看退出码
```
