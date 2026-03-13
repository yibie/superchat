import SwiftUI

struct AISettingsView: View {
    @State private var selectedProvider: AIProviderKind = .anthropic
    @State private var apiKey = ""
    @State private var baseURL = ""
    @State private var modelName = ""
    @State private var testStatus: TestStatus = .idle

    private enum TestStatus: Equatable {
        case idle
        case testing
        case success
        case failure(String)
    }

    private var isLocalProvider: Bool {
        AIProviderKind.localProviders.contains(selectedProvider)
    }

    var body: some View {
        Form {
            Section("Cloud Providers") {
                Picker("Provider", selection: $selectedProvider) {
                    ForEach(AIProviderKind.cloudProviders) { kind in
                        Text(kind.title).tag(kind)
                    }
                }
                .onChange(of: selectedProvider) { _, _ in loadSettings() }

                if !isLocalProvider {
                    SecureField("API Key", text: $apiKey)
                        .textFieldStyle(.roundedBorder)
                    TextField("Model (leave blank for default)", text: $modelName)
                        .textFieldStyle(.roundedBorder)
                }
            }

            Section("Local / Self-Hosted") {
                Picker("Provider", selection: $selectedProvider) {
                    ForEach(AIProviderKind.localProviders) { kind in
                        Text(kind.title).tag(kind)
                    }
                }
                .onChange(of: selectedProvider) { _, _ in loadSettings() }

                if isLocalProvider {
                    TextField("Base URL", text: $baseURL)
                        .textFieldStyle(.roundedBorder)
                    TextField("Model name", text: $modelName)
                        .textFieldStyle(.roundedBorder)
                    if selectedProvider == .openAICompat {
                        SecureField("API Key (optional)", text: $apiKey)
                            .textFieldStyle(.roundedBorder)
                    }
                }
            }

            Section {
                HStack {
                    Button("Save") {
                        saveSettings()
                    }
                    .buttonStyle(.borderedProminent)

                    Button("Test Connection") {
                        testConnection()
                    }
                    .disabled(testStatus == .testing)

                    switch testStatus {
                    case .idle:
                        EmptyView()
                    case .testing:
                        ProgressView().controlSize(.small)
                    case .success:
                        Label("Connected", systemImage: "checkmark.circle.fill")
                            .foregroundStyle(.green)
                            .font(.caption)
                    case .failure(let msg):
                        Label(msg, systemImage: "xmark.circle.fill")
                            .foregroundStyle(.red)
                            .font(.caption)
                            .lineLimit(2)
                    }
                }
            }
        }
        .formStyle(.grouped)
        .frame(width: 460, height: 400)
        .onAppear { loadSettings() }
    }

    private func loadSettings() {
        let prefix = selectedProvider.rawValue
        apiKey = KeychainHelper.read(key: "\(prefix).apiKey") ?? ""
        modelName = KeychainHelper.read(key: "\(prefix).model") ?? ""
        baseURL = KeychainHelper.read(key: "\(prefix).baseURL") ?? defaultBaseURL
        testStatus = .idle
    }

    private func saveSettings() {
        let prefix = selectedProvider.rawValue
        KeychainHelper.save(key: "\(prefix).apiKey", value: apiKey)
        KeychainHelper.save(key: "\(prefix).model", value: modelName.isEmpty ? defaultModel : modelName)
        if isLocalProvider {
            KeychainHelper.save(key: "\(prefix).baseURL", value: baseURL.isEmpty ? defaultBaseURL : baseURL)
        }
        KeychainHelper.save(key: "selectedProvider", value: prefix)
        NotificationCenter.default.post(name: .aiSettingsChanged, object: nil)
    }

    private var defaultModel: String {
        switch selectedProvider {
        case .anthropic: "claude-sonnet-4-5-20250514"
        case .openAI:    "gpt-4.1-mini"
        case .google:    "gemini-2.0-flash"
        case .groq:      "llama-3.3-70b-versatile"
        case .deepSeek:  "deepseek-chat"
        case .xai:       "grok-3-mini"
        case .ollama:    "llama3.2"
        case .lmStudio:  "local-model"
        default:         "default"
        }
    }

    private var defaultBaseURL: String {
        switch selectedProvider {
        case .ollama:      "http://localhost:11434/v1"
        case .lmStudio:    "http://localhost:1234/v1"
        case .openAICompat:"http://localhost:8000/v1"
        default:           ""
        }
    }

    private func testConnection() {
        saveSettings()
        testStatus = .testing
        Task {
            do {
                let provider = LLMProvider()
                try provider.configure()
                _ = try await provider.ping()
                testStatus = .success
            } catch {
                testStatus = .failure(error.localizedDescription)
            }
        }
    }
}

extension NSNotification.Name {
    static let aiSettingsChanged = NSNotification.Name("aiSettingsChanged")
}
