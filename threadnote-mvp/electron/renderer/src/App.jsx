import { ThemeProvider } from "./contexts/ThemeContext.jsx";
import { WorkbenchProvider } from "./contexts/WorkbenchContext.jsx";
import { NavigationProvider } from "./contexts/NavigationContext.jsx";
import { ErrorBoundary } from "./components/shared/ErrorBoundary.jsx";
import { AppShell } from "./components/shell/AppShell.jsx";
import { WorkspaceGate } from "./components/shell/WorkspaceGate.jsx";
import { QuickCaptureApp } from "./components/quick-capture/QuickCaptureApp.jsx";
import { SettingsWindow } from "./components/settings/SettingsWindow.jsx";

function resolveSurface() {
  if (typeof window === "undefined") {
    return "main";
  }
  return new URLSearchParams(window.location.search).get("surface") ?? "main";
}

export function App() {
  const surface = resolveSurface();
  return (
    <ErrorBoundary>
      <ThemeProvider>
        <WorkbenchProvider>
          {(workbench) =>
            surface === "quickCapture" ? (
              <QuickCaptureApp />
            ) : surface === "settings" ? (
              <SettingsWindow />
            ) : !workbench.workspace ? (
              <WorkspaceGate />
            ) : (
              <NavigationProvider>
                <AppShell />
              </NavigationProvider>
            )
          }
        </WorkbenchProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}
