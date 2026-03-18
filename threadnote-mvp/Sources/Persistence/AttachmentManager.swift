/// task007 — Copy dragged files into the workspace attachments directory.
/// Returns a relative path (attachments/<sha256>.<ext>) that is portable
/// when the .threadnote package is moved.

import Foundation
import CryptoKit

enum AttachmentManager {

    /// Copies `sourceURL` into `attachmentsURL`, using a SHA-256 content hash
    /// as the filename to deduplicate identical files.
    ///
    /// Returns the relative path string suitable for storing in an entry body,
    /// e.g. `"attachments/a3f9c1…b2.png"`.
    @discardableResult
    static func copyFile(_ sourceURL: URL, into attachmentsURL: URL) throws -> String {
        let fm = FileManager.default
        try fm.createDirectory(at: attachmentsURL, withIntermediateDirectories: true)

        let data = try Data(contentsOf: sourceURL)
        let hash = SHA256.hash(data: data)
        let hexDigest = hash.compactMap { String(format: "%02x", $0) }.joined()
        let ext = sourceURL.pathExtension.lowercased()
        let filename = ext.isEmpty ? hexDigest : "\(hexDigest).\(ext)"

        let destination = attachmentsURL.appending(component: filename)
        if !fm.fileExists(atPath: destination.path) {
            try fm.copyItem(at: sourceURL, to: destination)
        }

        return "attachments/\(filename)"
    }
}
