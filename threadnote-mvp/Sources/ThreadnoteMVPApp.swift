import SwiftUI

@main
struct ThreadnoteMVPApp: App {
    @State private var store = ThreadnoteStore()
    @State private var workspace = WorkspaceManager()
    @Environment(\.openWindow) private var openWindow
    @State private var showingSettings = false

    var body: some Scene {
        WindowGroup("Threadnote") {
            Group {
                if workspace.isConfigured {
                    ContentView()
                        .environment(store)
                        .sheet(isPresented: $showingSettings) {
                            AISettingsView()
                        }
                } else {
                    WorkspaceSetupView()
                }
            }
            .environment(workspace)
            .onAppear {
                if let url = workspace.databaseURL {
                    store.configure(with: url)
                }
            }
            .onChange(of: workspace.isConfigured) { _, configured in
                if configured, let url = workspace.databaseURL {
                    store.configure(with: url)
                }
            }
        }
        .defaultSize(width: 1280, height: 860)
        .commands {
            CommandGroup(replacing: .appSettings) {
                Button("Settings...") {
                    showingSettings = true
                }
                .keyboardShortcut(",", modifiers: .command)
            }
            CommandMenu("Capture") {
                Button("Open Quick Capture") {
                    store.beginCapture()
                    openWindow(id: "capture")
                }
                .keyboardShortcut("k", modifiers: [.command, .shift])
            }
        }

        WindowGroup("Quick Capture", id: "capture") {
            QuickCaptureView()
                .environment(store)
        }
        .windowStyle(.hiddenTitleBar)
        .defaultSize(width: 560, height: 380)
    }
}
