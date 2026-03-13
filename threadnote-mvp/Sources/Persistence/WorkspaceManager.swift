import Foundation
import AppKit
import UniformTypeIdentifiers

@Observable
@MainActor
final class WorkspaceManager {

    private(set) var workspaceURL: URL?

    var databaseURL: URL? { workspaceURL?.appending(component: "db.sqlite") }
    var attachmentsURL: URL? { workspaceURL?.appending(component: "attachments") }
    var isConfigured: Bool { workspaceURL != nil }

    private static let bookmarkKey = "workspace.bookmark"

    init() {
        restoreFromBookmark()
    }

    // MARK: - User actions

    /// Open NSSavePanel so the user names and places a new .threadnote workspace.
    func createWorkspace() {
        let panel = NSSavePanel()
        panel.title = "Create Threadnote Workspace"
        panel.message = "Choose where to save your Threadnote workspace."
        panel.nameFieldStringValue = "Threadnote"
        panel.canCreateDirectories = true
        if let type = UTType(filenameExtension: "threadnote") {
            panel.allowedContentTypes = [type]
        }

        guard panel.runModal() == .OK, let url = panel.url else { return }
        do {
            try openWorkspace(at: url)
        } catch {
            presentError(error, context: "creating workspace")
        }
    }

    /// Open NSOpenPanel so the user picks an existing .threadnote directory.
    func openExistingWorkspace() {
        let panel = NSOpenPanel()
        panel.title = "Open Threadnote Workspace"
        panel.message = "Select an existing .threadnote workspace directory."
        panel.canChooseFiles = false
        panel.canChooseDirectories = true
        panel.canCreateDirectories = false
        if let type = UTType(filenameExtension: "threadnote") {
            panel.allowedContentTypes = [type]
        }

        guard panel.runModal() == .OK, let url = panel.url else { return }
        do {
            try openWorkspace(at: url)
        } catch {
            presentError(error, context: "opening workspace")
        }
    }

    // MARK: - Internal

    private func openWorkspace(at url: URL) throws {
        try createDirectoryStructure(at: url)
        try saveBookmark(for: url)
        workspaceURL = url
    }

    private func createDirectoryStructure(at url: URL) throws {
        let fm = FileManager.default
        try fm.createDirectory(at: url, withIntermediateDirectories: true)
        let attachments = url.appending(component: "attachments")
        if !fm.fileExists(atPath: attachments.path) {
            try fm.createDirectory(at: attachments, withIntermediateDirectories: true)
        }
        // db.sqlite is created by GRDB on first open; we just ensure the directory exists.
    }

    private func saveBookmark(for url: URL) throws {
        let data = try url.bookmarkData(
            options: [],
            includingResourceValuesForKeys: nil,
            relativeTo: nil
        )
        UserDefaults.standard.set(data, forKey: Self.bookmarkKey)
    }

    private func restoreFromBookmark() {
        guard let data = UserDefaults.standard.data(forKey: Self.bookmarkKey) else { return }
        do {
            var isStale = false
            let url = try URL(
                resolvingBookmarkData: data,
                options: [],
                relativeTo: nil,
                bookmarkDataIsStale: &isStale
            )
            if isStale {
                try? saveBookmark(for: url)
            }
            // Verify directory still exists
            guard FileManager.default.fileExists(atPath: url.path) else {
                UserDefaults.standard.removeObject(forKey: Self.bookmarkKey)
                return
            }
            workspaceURL = url
        } catch {
            UserDefaults.standard.removeObject(forKey: Self.bookmarkKey)
        }
    }

    private func presentError(_ error: Error, context: String) {
        let alert = NSAlert(error: error)
        alert.messageText = "Workspace Error"
        alert.informativeText = "An error occurred while \(context): \(error.localizedDescription)"
        alert.runModal()
    }
}
