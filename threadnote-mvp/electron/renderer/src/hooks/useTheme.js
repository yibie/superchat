import { useState, useEffect, useCallback } from "react";

const STORAGE_KEY = "threadnote-theme";

function getSystemTheme() {
  return window.matchMedia?.("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}

function resolveTheme(preference) {
  if (preference === "system") return getSystemTheme();
  return preference;
}

export function useTheme() {
  const [preference, setPreference] = useState(() => {
    return localStorage.getItem(STORAGE_KEY) ?? "dark";
  });

  const resolved = resolveTheme(preference);

  useEffect(() => {
    const root = document.documentElement;
    root.classList.remove("dark", "light");
    root.classList.add(resolved);
  }, [resolved]);

  useEffect(() => {
    if (preference !== "system") return;
    const mq = window.matchMedia("(prefers-color-scheme: dark)");
    const onChange = () => setPreference("system");
    mq.addEventListener("change", onChange);
    return () => mq.removeEventListener("change", onChange);
  }, [preference]);

  const setTheme = useCallback((next) => {
    localStorage.setItem(STORAGE_KEY, next);
    setPreference(next);
  }, []);

  const cycle = useCallback(() => {
    const order = ["dark", "light", "system"];
    const i = order.indexOf(preference);
    setTheme(order[(i + 1) % order.length]);
  }, [preference, setTheme]);

  return { preference, resolved, setTheme, cycle };
}
