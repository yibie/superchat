import SwiftUI

// MARK: - Spacing

enum TNSpacing {
    static let xs: CGFloat = 4
    static let sm: CGFloat = 8
    static let md: CGFloat = 16
    static let lg: CGFloat = 24
    static let xl: CGFloat = 32
    static let xxl: CGFloat = 48
}

// MARK: - Typography

extension Font {
    static let tnPageTitle: Font = .system(size: 28, weight: .bold)
    static let tnSectionTitle: Font = .system(size: 17, weight: .semibold)
    static let tnBody: Font = .system(size: 17)
    static let tnCaption: Font = .system(size: 15)
    static let tnMicro: Font = .system(size: 13)
}

// MARK: - Colors

extension Color {
    /// Warm light gray canvas — the app's base layer
    static let tnBackground = Color(light: Color(red: 0.95, green: 0.95, blue: 0.96),
                                     dark: Color(nsColor: .windowBackgroundColor))
    /// White card surface floating on the gray canvas
    static let tnSurface = Color(light: .white,
                                  dark: Color(nsColor: .controlBackgroundColor))
    static let tnBorder = Color.primary.opacity(0.08)
    static let tnBorderSubtle = Color.primary.opacity(0.04)

    /// Convenience for light/dark adaptive colors
    init(light: Color, dark: Color) {
        self.init(nsColor: NSColor(name: nil, dynamicProvider: { appearance in
            let isDark = appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
            return isDark ? NSColor(dark) : NSColor(light)
        }))
    }
}

// MARK: - Corner Radii

enum TNCorner {
    static let sm: CGFloat = 6
    static let md: CGFloat = 12
    static let lg: CGFloat = 16
}

// MARK: - Shadow

extension View {
    func tnPopupShadow() -> some View {
        shadow(color: .black.opacity(0.10), radius: 16, y: 8)
    }
}

// MARK: - Thread Color

extension ThreadColor {
    var color: Color {
        switch self {
        case .rose:    Color(red: 0.90, green: 0.40, blue: 0.45)
        case .amber:   Color(red: 0.90, green: 0.65, blue: 0.20)
        case .lime:    Color(red: 0.55, green: 0.78, blue: 0.28)
        case .teal:    Color(red: 0.20, green: 0.70, blue: 0.64)
        case .sky:     Color(red: 0.28, green: 0.60, blue: 0.86)
        case .violet:  Color(red: 0.55, green: 0.35, blue: 0.85)
        case .fuchsia: Color(red: 0.85, green: 0.30, blue: 0.74)
        case .orange:  Color(red: 0.95, green: 0.50, blue: 0.24)
        case .emerald: Color(red: 0.24, green: 0.74, blue: 0.54)
        case .indigo:  Color(red: 0.35, green: 0.35, blue: 0.80)
        }
    }
}

// MARK: - Entry Kind Colors

extension EntryKind {
    var kindColor: Color {
        switch self {
        case .note: .secondary
        case .idea: Color(red: 0.75, green: 0.65, blue: 0.2)
        case .question: Color(red: 0.3, green: 0.5, blue: 0.8)
        case .claim: Color(red: 0.8, green: 0.55, blue: 0.25)
        case .evidence: Color(red: 0.3, green: 0.65, blue: 0.4)
        case .source: Color(red: 0.55, green: 0.4, blue: 0.7)
        case .comparison: Color(red: 0.35, green: 0.6, blue: 0.65)
        case .pattern: Color(red: 0.4, green: 0.35, blue: 0.7)
        case .plan: Color(red: 0.3, green: 0.6, blue: 0.6)
        case .decided: .accentColor
        case .solved: Color(red: 0.3, green: 0.65, blue: 0.4)
        case .verified: Color(red: 0.35, green: 0.65, blue: 0.55)
        case .dropped: Color(red: 0.75, green: 0.35, blue: 0.3)
        case .handoff, .anchorWritten: .secondary
        }
    }
}
