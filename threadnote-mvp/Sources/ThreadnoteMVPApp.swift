import SwiftUI

@main
struct ThreadnoteMVPApp: App {
    @State private var store = ThreadnoteStore()
    @Environment(\.openWindow) private var openWindow

    var body: some Scene {
        WindowGroup("Threadnote") {
            ContentView()
                .environment(store)
        }
        .defaultSize(width: 1280, height: 860)

        WindowGroup("Quick Capture", id: "capture") {
            QuickCaptureView()
                .environment(store)
        }
        .windowStyle(.hiddenTitleBar)
        .defaultSize(width: 560, height: 380)
        .commands {
            CommandMenu("Capture") {
                Button("Open Quick Capture") {
                    store.beginCapture()
                    openWindow(id: "capture")
                }
                .keyboardShortcut("k", modifiers: [.command, .shift])
            }
        }
    }
}
