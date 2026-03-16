import { createContext, useContext } from "react";
import { useTheme } from "../hooks/useTheme.js";

const ThemeContext = createContext(null);

export function ThemeProvider({ children }) {
  const theme = useTheme();
  return <ThemeContext value={theme}>{children}</ThemeContext>;
}

export function useThemeContext() {
  const ctx = useContext(ThemeContext);
  if (!ctx) throw new Error("useThemeContext must be used within ThemeProvider");
  return ctx;
}
