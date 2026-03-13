import SwiftUI

/// Shown on first launch (or when bookmark is lost) to create or open a workspace.
struct WorkspaceSetupView: View {
    @Environment(WorkspaceManager.self) private var workspace

    var body: some View {
        VStack(spacing: 32) {
            VStack(spacing: 8) {
                Image(systemName: "tray.full")
                    .font(.system(size: 48, weight: .light))
                    .foregroundStyle(.secondary)
                Text("Welcome to Threadnote")
                    .font(.title2)
                    .fontWeight(.semibold)
                Text("Choose where to store your threads and notes.")
                    .font(.body)
                    .foregroundStyle(.secondary)
                    .multilineTextAlignment(.center)
            }

            VStack(spacing: 12) {
                Button(action: workspace.createWorkspace) {
                    Label("Create New Workspace", systemImage: "plus.circle")
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 4)
                }
                .buttonStyle(.borderedProminent)
                .controlSize(.large)

                Button(action: workspace.openExistingWorkspace) {
                    Label("Open Existing Workspace", systemImage: "folder")
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 4)
                }
                .buttonStyle(.bordered)
                .controlSize(.large)
            }
            .frame(maxWidth: 280)
        }
        .padding(48)
        .frame(minWidth: 480, minHeight: 340)
    }
}
