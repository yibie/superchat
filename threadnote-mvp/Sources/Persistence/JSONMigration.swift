/// task006 — One-time import of legacy snapshot.json into SQLite.
/// Runs inside PersistenceStore.init() before any data is loaded.

import Foundation

enum JSONMigration {

    static let metadataKey = "json_migration_done"

    /// Imports snapshot.json into the SQLite store if not already done.
    /// - No-op if migration already ran (metadata key present).
    /// - No-op if snapshot.json does not exist (fresh install).
    static func runIfNeeded(into store: PersistenceStore) throws {
        // Already migrated in a previous session.
        if let done = try store.metadataValue(for: metadataKey), done == "1" { return }

        guard let snapshotURL = legacySnapshotURL,
              FileManager.default.fileExists(atPath: snapshotURL.path) else {
            // No JSON to migrate; mark done so we never check again.
            try store.setMetadata("1", for: metadataKey)
            return
        }

        do {
            let data = try Data(contentsOf: snapshotURL)
            let decoder = JSONDecoder()
            decoder.dateDecodingStrategy = .iso8601
            let snapshot = try decoder.decode(AppSnapshot.self, from: data)

            try store.saveSnapshot(snapshot)
            try store.setMetadata("1", for: metadataKey)

            // Backup original file.
            let backupURL = snapshotURL.deletingLastPathComponent()
                .appending(component: "snapshot.json.bak")
            try? FileManager.default.moveItem(at: snapshotURL, to: backupURL)

            print("[JSONMigration] Imported \(snapshot.threads.count) threads, " +
                  "\(snapshot.entries.count) entries from snapshot.json")
        } catch {
            // Migration failed: mark it done anyway so we don't loop on bad JSON.
            try? store.setMetadata("1", for: metadataKey)
            print("[JSONMigration] Failed to import snapshot.json: \(error)")
        }
    }

    // MARK: - Private

    private static var legacySnapshotURL: URL? {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)
            .first?
            .appending(component: "ThreadnoteMVP")
            .appending(component: "snapshot.json")
    }
}
