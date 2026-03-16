import { createContext, useContext } from "react";
import { useNavigation } from "../hooks/useNavigation.js";

const NavigationContext = createContext(null);

export function NavigationProvider({ children }) {
  const nav = useNavigation();
  return <NavigationContext value={nav}>{children}</NavigationContext>;
}

export function useNavigationContext() {
  const ctx = useContext(NavigationContext);
  if (!ctx) throw new Error("useNavigationContext must be used within NavigationProvider");
  return ctx;
}
