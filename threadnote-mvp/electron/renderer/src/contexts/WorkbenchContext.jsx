import { createContext, useContext } from "react";
import { useWorkbench } from "../hooks/useWorkbench.js";

const WorkbenchContext = createContext(null);

export function WorkbenchProvider({ children }) {
  const workbench = useWorkbench();
  return (
    <WorkbenchContext value={workbench}>
      {typeof children === "function" ? children(workbench) : children}
    </WorkbenchContext>
  );
}

export function useWorkbenchContext() {
  const ctx = useContext(WorkbenchContext);
  if (!ctx) throw new Error("useWorkbenchContext must be used within WorkbenchProvider");
  return ctx;
}
