import Foundation
import NaturalLanguage

/// Tokenizes text for search, handling CJK scripts via NLTokenizer with bigram fallback.
func tokenizeForSearch(_ text: String) -> [String] {
    let lowered = text.lowercased()
    let tokenizer = NLTokenizer(unit: .word)
    tokenizer.string = lowered

    var results: [String] = []
    tokenizer.enumerateTokens(in: lowered.startIndex..<lowered.endIndex) { range, _ in
        let word = String(lowered[range])
        if word.containsCJK {
            if word.count > 2 {
                // Emit overlapping bigrams for long CJK segments
                let chars = Array(word)
                for i in 0..<(chars.count - 1) {
                    results.append(String(chars[i...i+1]))
                }
            } else if word.count > 0 {
                results.append(word)
            }
        } else if word.count > 2 {
            results.append(word)
        }
        return true
    }
    return results
}

private extension String {
    var containsCJK: Bool {
        contains { c in
            guard let scalar = c.unicodeScalars.first else { return false }
            let v = scalar.value
            // CJK Unified Ideographs + Extension A + CJK Compat Ideographs
            return (0x4E00...0x9FFF).contains(v)
                || (0x3400...0x4DBF).contains(v)
                || (0xF900...0xFAFF).contains(v)
                // CJK Unified Ideographs Extension B+
                || (0x20000...0x2FA1F).contains(v)
                // Hangul Syllables
                || (0xAC00...0xD7AF).contains(v)
                // Hiragana + Katakana
                || (0x3040...0x30FF).contains(v)
        }
    }
}
