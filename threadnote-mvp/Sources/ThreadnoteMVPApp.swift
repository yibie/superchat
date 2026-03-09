import SwiftUI

@main
struct ThreadnoteMVPApp: App {
    @State private var store = ThreadnoteStore()

    var body: some Scene {
        WindowGroup("Threadnote") {
            ContentView(store: store)
        }
        .defaultSize(width: 1280, height: 860)

        WindowGroup("Quick Capture", id: "capture") {
            QuickCaptureView(store: store)
        }
        .windowStyle(.hiddenTitleBar)
        .defaultSize(width: 560, height: 380)

        MenuBarExtra("Threadnote", systemImage: "waveform.and.magnifyingglass") {
            VStack(alignment: .leading, spacing: 12) {
                Text("Threadnote")
                    .font(.headline)
                Button("Quick Capture") {
                    store.beginCapture()
                    openWindow(id: "capture")
                }
                Text("Thread state saves automatically when you leave.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .padding(12)
            .frame(width: 240)
        }
    }

    @Environment(\.openWindow) private var openWindow
}
