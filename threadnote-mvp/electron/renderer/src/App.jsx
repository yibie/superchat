import { ThemeProvider } from "./contexts/ThemeContext.jsx";
import { WorkbenchProvider } from "./contexts/WorkbenchContext.jsx";
import { NavigationProvider } from "./contexts/NavigationContext.jsx";
import { ErrorBoundary } from "./components/shared/ErrorBoundary.jsx";
import { AppShell } from "./components/shell/AppShell.jsx";
import { WorkspaceGate } from "./components/shell/WorkspaceGate.jsx";

export function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider>
        <WorkbenchProvider>
          {(workbench) =>
            !workbench.workspace ? (
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
