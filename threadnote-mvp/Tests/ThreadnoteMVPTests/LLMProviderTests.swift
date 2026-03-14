import XCTest
import SwiftAISDK
@testable import ThreadnoteMVP

final class LLMProviderTests: XCTestCase {
    func testOllamaChatModelNormalizesBaseURLAndBuildsNativeRequest() async throws {
        final class RequestBox: @unchecked Sendable {
            var request: URLRequest?
        }

        let requestBox = RequestBox()
        let responseData = """
        {
          "model": "qwen3.5:0.8b",
          "created_at": "2026-03-14T00:00:00Z",
          "message": {
            "role": "assistant",
            "content": "pong"
          },
          "done": true,
          "done_reason": "stop",
          "prompt_eval_count": 12,
          "eval_count": 1
        }
        """.data(using: .utf8)!

        let model = OllamaChatModel(
            modelId: "qwen3.5:0.8b",
            baseURL: "http://localhost:11434/v1",
            requestTimeout: 90,
            keepAlive: "30m"
        ) { request in
            requestBox.request = request
            let response = try XCTUnwrap(
                HTTPURLResponse(
                    url: request.url ?? URL(string: "http://localhost")!,
                    statusCode: 200,
                    httpVersion: nil,
                    headerFields: ["Content-Type": "application/json"]
                )
            )
            return (responseData, response)
        }

        XCTAssertEqual(model.baseURL, "http://localhost:11434")

        let result = try await model.doGenerate(
            options: LanguageModelV3CallOptions(
                prompt: [LanguageModelV3Message.user(
                        content: [.text(.init(text: "Reply with exactly: pong"))],
                        providerOptions: nil
                    )]
            )
        )

        let request = try XCTUnwrap(requestBox.request)
        XCTAssertEqual(request.url?.absoluteString, "http://localhost:11434/api/chat")
        XCTAssertEqual(request.timeoutInterval, 90, accuracy: 0.001)

        let bodyData = try XCTUnwrap(request.httpBody)
        let body = try XCTUnwrap(JSONSerialization.jsonObject(with: bodyData) as? [String: Any])
        XCTAssertEqual(body["model"] as? String, "qwen3.5:0.8b")
        XCTAssertEqual(body["think"] as? Bool, false)
        XCTAssertEqual(body["stream"] as? Bool, false)
        XCTAssertEqual(body["keep_alive"] as? String, "30m")

        let messages = try XCTUnwrap(body["messages"] as? [[String: String]])
        XCTAssertEqual(messages.count, 1)
        XCTAssertEqual(messages[0]["role"], "user")
        XCTAssertEqual(messages[0]["content"], "Reply with exactly: pong")

        XCTAssertEqual(result.content, [LanguageModelV3Content.text(LanguageModelV3Text(text: "pong"))])
        XCTAssertEqual(result.finishReason, LanguageModelV3FinishReason(unified: .stop, raw: "stop"))
        XCTAssertEqual(result.usage.inputTokens.total, 12)
        XCTAssertEqual(result.usage.outputTokens.total, 1)
    }

    func testOllamaChatModelWrapsRequestTimeoutWithLocalDebugHint() async {
        let model = OllamaChatModel(
            modelId: "qwen3.5:27b",
            baseURL: "http://localhost:11434/v1",
            requestTimeout: 75
        ) { _ in
            throw URLError(.timedOut)
        }

        await XCTAssertThrowsErrorAsync(try await model.doGenerate(
            options: LanguageModelV3CallOptions(
                prompt: [LanguageModelV3Message.user(
                        content: [.text(.init(text: "ping"))],
                        providerOptions: nil
                    )]
            )
        )) { error in
            XCTAssertEqual(
                error.localizedDescription,
                "Ollama request to http://localhost:11434/api/chat for model qwen3.5:27b timed out after 75s. The local model may still be loading; try a smaller model or keep it warm before retrying."
            )
        }
    }

    func testProviderKindDefaultsStayAlignedForOllama() {
        XCTAssertTrue(AIProviderKind.ollama.isLocalProvider)
        XCTAssertEqual(AIProviderKind.ollama.defaultModel, "qwen3.5:4b")
        XCTAssertEqual(AIProviderKind.ollama.defaultBaseURL, "http://localhost:11434/v1")
    }

    func testRelationKindParsingNormalizesModelOutput() {
        XCTAssertEqual(LLMProvider.relationKind(from: "supports"), .supports)
        XCTAssertEqual(LLMProvider.relationKind(from: "OPPOSES"), .opposes)
        XCTAssertEqual(LLMProvider.relationKind(from: " answers "), .answers)
        XCTAssertEqual(LLMProvider.relationKind(from: "unknown"), .informs)
    }

    func testPresentationBlockParsingNormalizesModelOutput() {
        XCTAssertEqual(LLMProvider.blockKind(from: "principles"), .principles)
        XCTAssertEqual(LLMProvider.blockKind(from: " nextMove "), .nextMove)
        XCTAssertNil(LLMProvider.blockKind(from: "unknown"))

        XCTAssertEqual(LLMProvider.blockTone(from: "warning"), .warning)
        XCTAssertEqual(LLMProvider.blockTone(from: " ACCENT "), .accent)
        XCTAssertNil(LLMProvider.blockTone(from: ""))
    }

    // MARK: - Live Ollama Integration Test

    /// Run this test with a local Ollama instance running.
    /// It verifies the full chain: UserDefaults → configure() → ping().
    @MainActor
    func testOllamaLiveConnectivity() async throws {
        // 1. Write known-good settings into UserDefaults
        let prefix = "com.chenyibin.threadnote."
        UserDefaults.standard.set("ollama", forKey: prefix + "selectedProvider")
        UserDefaults.standard.set("qwen3.5:0.8b", forKey: prefix + "ollama.model")
        UserDefaults.standard.set("http://localhost:11434/v1", forKey: prefix + "ollama.baseURL")
        UserDefaults.standard.set("", forKey: prefix + "ollama.apiKey")

        // 2. Verify they round-trip via KeychainHelper
        let readProvider = KeychainHelper.read(key: "selectedProvider")
        let readModel = KeychainHelper.read(key: "ollama.model")
        let readBaseURL = KeychainHelper.read(key: "ollama.baseURL")
        print("[TEST] selectedProvider = \(readProvider ?? "nil")")
        print("[TEST] ollama.model = \(readModel ?? "nil")")
        print("[TEST] ollama.baseURL = \(readBaseURL ?? "nil")")
        XCTAssertEqual(readProvider, "ollama")
        XCTAssertEqual(readModel, "qwen3.5:0.8b")
        XCTAssertEqual(readBaseURL, "http://localhost:11434/v1")

        // 3. Configure LLMProvider
        let provider = LLMProvider()
        try provider.configure()
        print("[TEST] isConfigured = \(provider.isConfigured)")
        print("[TEST] activeModelID = \(provider.activeModelID ?? "nil")")
        print("[TEST] backendLabel = \(provider.backendLabel)")
        XCTAssertTrue(provider.isConfigured, "LLMProvider should be configured after reading Ollama settings")
        XCTAssertEqual(provider.activeModelID, "qwen3.5:0.8b")

        // 4. Ping Ollama
        let pong = try await provider.ping()
        print("[TEST] Ping response = \(pong)")
        XCTAssertEqual(pong.trimmingCharacters(in: .whitespacesAndNewlines), "pong")
    }
}

private func XCTAssertThrowsErrorAsync<T>(
    _ expression: @autoclosure () async throws -> T,
    _ errorHandler: (Error) -> Void,
    file: StaticString = #filePath,
    line: UInt = #line
) async {
    do {
        _ = try await expression()
        XCTFail("Expected expression to throw an error", file: file, line: line)
    } catch {
        errorHandler(error)
    }
}
