import Foundation
import SwiftAISDK

struct OllamaChatModel: LanguageModelV3 {
    typealias Transport = @Sendable (URLRequest) async throws -> (Data, URLResponse)

    let provider = "ollama"
    let modelId: String
    let baseURL: String

    private let transport: Transport

    init(
        modelId: String,
        baseURL: String,
        transport: @escaping Transport = { request in
            try await URLSession.shared.data(for: request)
        }
    ) {
        self.modelId = modelId
        self.baseURL = Self.normalizeBaseURL(baseURL)
        self.transport = transport
    }

    func doGenerate(options: LanguageModelV3CallOptions) async throws -> LanguageModelV3GenerateResult {
        try checkCancellation(options.abortSignal)

        let requestBody = try makeRequestBody(from: options)
        let responseURL = try chatURL()
        let requestData = try JSONSerialization.data(withJSONObject: requestBody, options: [])

        var request = URLRequest(url: responseURL)
        request.httpMethod = "POST"
        request.httpBody = requestData
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        options.headers?.forEach { request.setValue($0.value, forHTTPHeaderField: $0.key) }

        let startedAt = Date()
        let (data, rawResponse) = try await transport(request)
        try checkCancellation(options.abortSignal)

        guard let httpResponse = rawResponse as? HTTPURLResponse else {
            throw OllamaError.invalidResponse("Expected HTTPURLResponse.")
        }

        let rawBody = try jsonObject(from: data)
        guard (200 ..< 300).contains(httpResponse.statusCode) else {
            throw OllamaError.httpError(
                statusCode: httpResponse.statusCode,
                body: String(describing: rawBody)
            )
        }

        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        let response = try decoder.decode(OllamaChatResponse.self, from: data)

        let finishReason = LanguageModelV3FinishReason(
            unified: mapFinishReason(response.doneReason),
            raw: response.doneReason
        )
        let usage = LanguageModelV3Usage(
            inputTokens: .init(total: response.promptEvalCount, noCache: response.promptEvalCount),
            outputTokens: .init(total: response.evalCount, text: response.evalCount),
            raw: nil
        )

        return LanguageModelV3GenerateResult(
            content: [.text(LanguageModelV3Text(text: response.message.content))],
            finishReason: finishReason,
            usage: usage,
            request: LanguageModelV3RequestInfo(body: requestBody),
            response: LanguageModelV3ResponseInfo(
                id: response.model,
                timestamp: response.createdAt ?? startedAt,
                modelId: response.model,
                headers: responseHeaders(from: httpResponse),
                body: rawBody
            ),
            warnings: []
        )
    }

    func doStream(options: LanguageModelV3CallOptions) async throws -> LanguageModelV3StreamResult {
        throw OllamaError.streamingNotSupported
    }

    static func normalizeBaseURL(_ baseURL: String) -> String {
        var normalized = baseURL.trimmingCharacters(in: .whitespacesAndNewlines)
        while normalized.hasSuffix("/") {
            normalized.removeLast()
        }
        if normalized.hasSuffix("/v1") {
            normalized.removeLast(3)
        }
        return normalized
    }

    private func chatURL() throws -> URL {
        guard let url = URL(string: baseURL + "/api/chat") else {
            throw OllamaError.invalidResponse("Invalid Ollama base URL: \(baseURL)")
        }
        return url
    }

    private func makeRequestBody(from options: LanguageModelV3CallOptions) throws -> [String: Any] {
        var body: [String: Any] = [
            "model": modelId,
            "messages": try makeMessages(from: options.prompt),
            "stream": false,
            "think": false,
        ]

        if let format = try makeFormat(from: options.responseFormat) {
            body["format"] = format
        }

        let nativeOptions = makeNativeOptions(from: options)
        if !nativeOptions.isEmpty {
            body["options"] = nativeOptions
        }

        return body
    }

    private func makeMessages(from prompt: LanguageModelV3Prompt) throws -> [[String: String]] {
        try prompt.map { message in
            switch message {
            case let .system(content, _):
                return ["role": "system", "content": content]
            case let .user(content, _):
                return ["role": "user", "content": try flattenUserParts(content)]
            case let .assistant(content, _):
                return ["role": "assistant", "content": try flattenAssistantParts(content)]
            case let .tool(content, _):
                return ["role": "tool", "content": flattenToolParts(content)]
            }
        }
    }

    private func flattenUserParts(_ parts: [LanguageModelV3UserMessagePart]) throws -> String {
        try parts.map { part in
            switch part {
            case let .text(textPart):
                return textPart.text
            case let .file(filePart):
                throw OllamaError.unsupportedPromptContent("User file part: \(filePart.mediaType)")
            }
        }.joined(separator: "\n")
    }

    private func flattenAssistantParts(_ parts: [LanguageModelV3MessagePart]) throws -> String {
        try parts.map { part in
            switch part {
            case let .text(textPart):
                return textPart.text
            case let .reasoning(reasoningPart):
                return reasoningPart.text
            case let .toolCall(toolCall):
                return "Tool call \(toolCall.toolName): \(stringify(json: toolCall.input))"
            case let .toolResult(toolResult):
                return "Tool result \(toolResult.toolName): \(stringify(toolResult: toolResult.output))"
            case let .file(filePart):
                throw OllamaError.unsupportedPromptContent("Assistant file part: \(filePart.mediaType)")
            }
        }.joined(separator: "\n")
    }

    private func flattenToolParts(_ parts: [LanguageModelV3ToolMessagePart]) -> String {
        parts.map { part in
            switch part {
            case let .toolResult(toolResult):
                return "Tool result \(toolResult.toolName): \(stringify(toolResult: toolResult.output))"
            case let .toolApprovalResponse(response):
                return "Tool approval \(response.approvalId): \(response.approved ? "approved" : "denied")"
            }
        }.joined(separator: "\n")
    }

    private func makeFormat(from responseFormat: LanguageModelV3ResponseFormat?) throws -> Any? {
        switch responseFormat {
        case .none, .text:
            return nil
        case let .json(schema, _, _):
            guard let schema else { return "json" }
            return try jsonValueToNative(schema)
        }
    }

    private func makeNativeOptions(from options: LanguageModelV3CallOptions) -> [String: Any] {
        var nativeOptions: [String: Any] = [:]

        if let temperature = options.temperature {
            nativeOptions["temperature"] = temperature
        }
        if let topP = options.topP {
            nativeOptions["top_p"] = topP
        }
        if let topK = options.topK {
            nativeOptions["top_k"] = topK
        }
        if let maxOutputTokens = options.maxOutputTokens {
            nativeOptions["num_predict"] = maxOutputTokens
        }
        if let stopSequences = options.stopSequences, !stopSequences.isEmpty {
            nativeOptions["stop"] = stopSequences
        }
        if let seed = options.seed {
            nativeOptions["seed"] = seed
        }

        return nativeOptions
    }

    private func responseHeaders(from response: HTTPURLResponse) -> SharedV3Headers {
        response.allHeaderFields.reduce(into: [:]) { result, entry in
            guard let key = entry.key as? String else { return }
            result[key] = String(describing: entry.value)
        }
    }

    private func jsonObject(from data: Data) throws -> Any {
        do {
            return try JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
        } catch {
            if let body = String(data: data, encoding: .utf8), !body.isEmpty {
                return body
            }
            throw error
        }
    }

    private func stringify(json value: JSONValue) -> String {
        do {
            let native = try jsonValueToNative(value)
            guard JSONSerialization.isValidJSONObject(native),
                  let data = try? JSONSerialization.data(withJSONObject: native, options: [.sortedKeys]),
                  let string = String(data: data, encoding: .utf8) else {
                return String(describing: native)
            }
            return string
        } catch {
            return String(describing: value)
        }
    }

    private func stringify(toolResult output: LanguageModelV3ToolResultOutput) -> String {
        switch output {
        case let .text(value, _), let .errorText(value, _):
            return value
        case let .json(value, _), let .errorJson(value, _):
            return stringify(json: value)
        case let .executionDenied(reason, _):
            return reason ?? "execution denied"
        case let .content(value, _):
            return value.map { part in
                switch part {
                case let .text(text):
                    return text
                case let .media(_, mediaType):
                    return "[media: \(mediaType)]"
                }
            }.joined(separator: "\n")
        }
    }

    private func mapFinishReason(_ rawValue: String?) -> LanguageModelV3FinishReason.Unified {
        switch rawValue?.lowercased() {
        case "stop", "end_turn":
            return .stop
        case "length":
            return .length
        case "tool_calls":
            return .toolCalls
        case "content_filter":
            return .contentFilter
        case "error":
            return .error
        default:
            return .other
        }
    }

    private func checkCancellation(_ abortSignal: (@Sendable () -> Bool)?) throws {
        try Task.checkCancellation()
        if abortSignal?() == true {
            throw CancellationError()
        }
    }

    private func jsonValueToNative(_ value: JSONValue) throws -> Any {
        switch value {
        case .null:
            return NSNull()
        case let .bool(bool):
            return bool
        case let .number(number):
            return number
        case let .string(string):
            return string
        case let .array(array):
            return try array.map(jsonValueToNative)
        case let .object(object):
            return try object.mapValues(jsonValueToNative)
        }
    }
}

extension OllamaChatModel {
    struct OllamaChatResponse: Decodable {
        let model: String?
        let createdAt: Date?
        let message: OllamaChatMessage
        let doneReason: String?
        let promptEvalCount: Int?
        let evalCount: Int?

        private enum CodingKeys: String, CodingKey {
            case model
            case createdAt = "created_at"
            case message
            case doneReason = "done_reason"
            case promptEvalCount = "prompt_eval_count"
            case evalCount = "eval_count"
        }
    }

    struct OllamaChatMessage: Decodable {
        let role: String
        let content: String
        let thinking: String?
    }

    enum OllamaError: LocalizedError {
        case httpError(statusCode: Int, body: String)
        case invalidResponse(String)
        case streamingNotSupported
        case unsupportedPromptContent(String)

        var errorDescription: String? {
            switch self {
            case let .httpError(statusCode, body):
                return "Ollama HTTP \(statusCode): \(body)"
            case let .invalidResponse(message):
                return "Invalid Ollama response: \(message)"
            case .streamingNotSupported:
                return "Ollama native streaming is not implemented for Threadnote."
            case let .unsupportedPromptContent(description):
                return "Ollama prompt contains unsupported content: \(description)"
            }
        }
    }
}
