import { createContext, useContext } from "react";
import { useNavigation } from "../hooks/useNavigation.js";
import { SURFACES } from "../lib/constants.js";

const noop = () => {};
const fallbackNavigation = {
  surface: SURFACES.STREAM,
  selectedThreadID: null,
  inspectorOpen: false,
  threadInspectorTab: "restart",
  focusedEntryTarget: null,
  navigate: noop,
  goBack: noop,
  goForward: noop,
  openThread: noop,
  canGoBack: false,
  canGoForward: false,
  goToStream: noop,
  goToResources: noop,
  toggleInspector: noop,
  setInspectorOpen: noop,
  focusEntry: noop,
  clearFocusedEntry: noop,
  setThreadInspectorTab: noop,
  showThreadInspectorTab: noop
};

const NavigationContext = globalThis.__threadnoteNavigationContext
  ?? createContext(fallbackNavigation);

if (!globalThis.__threadnoteNavigationContext) {
  globalThis.__threadnoteNavigationContext = NavigationContext;
}

export function NavigationProvider({ children }) {
  const nav = useNavigation();
  return <NavigationContext.Provider value={nav}>{children}</NavigationContext.Provider>;
}

export function useNavigationContext() {
  const ctx = useContext(NavigationContext);
  return ctx ?? fallbackNavigation;
}
