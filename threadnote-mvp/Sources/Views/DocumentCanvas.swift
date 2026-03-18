import SwiftUI

struct DocumentCanvas<Content: View>: View {
    @ViewBuilder let content: Content

    var body: some View {
        ScrollView {
            content
                .frame(maxWidth: 720)
                .frame(maxWidth: .infinity)
                .padding(.horizontal, TNSpacing.xl)
                .padding(.vertical, TNSpacing.lg)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color.tnBackground)
    }
}
