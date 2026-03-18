import Foundation
import Security

// API keys are stored in UserDefaults (not Keychain) to avoid repeated
// system authorization prompts in unsigned development builds.
// For a signed App Store release, migrate to Keychain with proper entitlements.

enum KeychainHelper {
    private static let prefix = "com.chenyibin.threadnote."

    static func save(key: String, value: String) {
        UserDefaults.standard.set(value, forKey: prefix + key)
    }

    static func read(key: String) -> String? {
        UserDefaults.standard.string(forKey: prefix + key)
    }

    static func delete(key: String) {
        UserDefaults.standard.removeObject(forKey: prefix + key)
    }
}
