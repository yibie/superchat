import SwiftUI

struct AISettingsView: View {
    @State private var selectedProvider: AIProviderKind = .anthropic
    @State private var apiKey = ""
    @State private var modelName = ""
    @State private var testStatus: TestStatus = .idle

    private enum TestStatus: Equatable {
        case idle
        case testing
        case success
        case failure(String)
    }

    var body: some View {
        Form {
            Section("AI Provider") {
                Picker("Provider", selection: $selectedProvider) {
                    ForEach(AIProviderKind.cloudProviders) { kind in
                        Text(kind.title).tag(kind)
                    }
                }
                .onChange(of: selectedProvider) { _, _ in
                    loadSettings()
                }

                SecureField("API Key", text: $apiKey)
                    .textFieldStyle(.roundedBorder)

                TextField("Model", text: $modelName)
                    .textFieldStyle(.roundedBorder)

                HStack {
                    Button("Test Connection") {
                        testConnection()
                    }
                    .disabled(apiKey.isEmpty || testStatus == .testing)

                    switch testStatus {
                    case .idle:
                        EmptyView()
                    case .testing:
                        ProgressView()
                            .controlSize(.small)
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

            Section {
                Button("Save") {
                    saveSettings()
                }
                .buttonStyle(.borderedProminent)
                .disabled(apiKey.isEmpty)
            }
        }
        .formStyle(.grouped)
        .frame(width: 440, height: 300)
        .onAppear { loadSettings() }
    }

    private func loadSettings() {
        let prefix = selectedProvider.rawValue
        apiKey = KeychainHelper.read(key: "\(prefix).apiKey") ?? ""
        modelName = KeychainHelper.read(key: "\(prefix).model") ?? defaultModel
        testStatus = .idle
    }

    private func saveSettings() {
        let prefix = selectedProvider.rawValue
        KeychainHelper.save(key: "\(prefix).apiKey", value: apiKey)
        let model = modelName.isEmpty ? defaultModel : modelName
        KeychainHelper.save(key: "\(prefix).model", value: model)
        KeychainHelper.save(key: "selectedProvider", value: prefix)
    }

    private var defaultModel: String {
        switch selectedProvider {
        case .anthropic: "claude-sonnet-4-5-20250514"
        case .openAI:    "gpt-4.1-mini"
        case .google:    "gemini-2.0-flash"
        case .groq:      "llama-3.3-70b-versatile"
        case .deepSeek:  "deepseek-chat"
        case .xai:       "grok-3-mini"
        default:         ""
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
