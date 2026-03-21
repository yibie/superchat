import { contextBridge, ipcRenderer, webUtils } from "electron";

contextBridge.exposeInMainWorld("threadnoteDesktop", {
  getShellState: () => ipcRenderer.invoke("desktop:get-shell-state"),

  openSettings: () => ipcRenderer.invoke("desktop:open-settings"),
  closeWindow: () => ipcRenderer.invoke("desktop:close-window"),
  getWorkbenchState: () => ipcRenderer.invoke("app:get-workbench-state"),
  repairWorkspaceRelations: () => ipcRenderer.invoke("app:repair-workspace-relations"),
  getStreamPage: (payload) => ipcRenderer.invoke("app:get-stream-page", payload),
  getThreadPage: (payload) => ipcRenderer.invoke("app:get-thread-page", payload),
  getShortcutSettings: () => ipcRenderer.invoke("app:get-shortcut-settings"),
  updateShortcutSetting: (payload) => ipcRenderer.invoke("app:update-shortcut-setting", payload),
  clearShortcutSetting: (actionId) => ipcRenderer.invoke("app:clear-shortcut-setting", actionId),
  openSettingsWindow: () => ipcRenderer.invoke("app:open-settings-window"),
  openQuickCapture: (payload) => ipcRenderer.invoke("app:open-quick-capture", payload),
  closeQuickCapture: () => ipcRenderer.invoke("app:close-quick-capture"),
  resizeQuickCapture: (payload) => ipcRenderer.invoke("app:resize-quick-capture", payload),
  importFromClipboard: () => ipcRenderer.invoke("app:import-from-clipboard"),
  submitQuickCapture: (payload) => ipcRenderer.invoke("app:submit-quick-capture", payload),
  createWorkspace: () => ipcRenderer.invoke("app:create-workspace"),
  openWorkspace: () => ipcRenderer.invoke("app:open-workspace"),
  createThread: (payload) => ipcRenderer.invoke("app:create-thread", payload),
  createThreadFromEntry: (payload) => ipcRenderer.invoke("app:create-thread-from-entry", payload),
  archiveThread: (threadID) => ipcRenderer.invoke("app:archive-thread", threadID),
  updateThreadTitle: (payload) => ipcRenderer.invoke("app:update-thread-title", payload),
  openThread: (threadID) => ipcRenderer.invoke("app:open-thread", threadID),
  prepareThread: (payload) => ipcRenderer.invoke("app:prepare-thread", payload),
  pingAIProvider: () => ipcRenderer.invoke("app:ping-ai-provider"),
  getAIProviderConfig: () => ipcRenderer.invoke("app:get-ai-provider-config"),
  saveAIProviderConfig: (payload) => ipcRenderer.invoke("app:save-ai-provider-config", payload),
  testAIProvider: (payload) => ipcRenderer.invoke("app:test-ai-provider", payload),
  appendReply: (payload) => ipcRenderer.invoke("app:append-reply", payload),
  updateEntryText: (payload) => ipcRenderer.invoke("app:update-entry-text", payload),
  updateEntryKind: (payload) => ipcRenderer.invoke("app:update-entry-kind", payload),
  updateEntryStatus: (payload) => ipcRenderer.invoke("app:update-entry-status", payload),
  deleteEntry: (entryID) => ipcRenderer.invoke("app:delete-entry", entryID),
  routeEntryToThread: (payload) => ipcRenderer.invoke("app:route-entry-to-thread", payload),
  openLocator: (locator) => ipcRenderer.invoke("app:open-locator", locator),
  getEntryRichPreview: (entryID) => ipcRenderer.invoke("app:get-entry-rich-preview", entryID),
  getLocatorRichPreview: (locator) => ipcRenderer.invoke("app:get-locator-rich-preview", locator),
  submitCapture: (payload) => ipcRenderer.invoke("app:submit-capture", payload),
  copyAttachment: (filePath) => ipcRenderer.invoke("app:copy-attachment", filePath),
  writeAttachmentBuffer: (payload) => ipcRenderer.invoke("app:write-attachment-buffer", payload),
  getFilePath: (file) => webUtils.getPathForFile(file),
  onOpenSettings: (callback) => {
    const listener = () => callback?.();
    ipcRenderer.on("desktop:open-settings", listener);
    return () => ipcRenderer.removeListener("desktop:open-settings", listener);
  },
  onShortcutSettingsUpdated: (callback) => {
    const listener = (_event, payload) => callback?.(payload);
    ipcRenderer.on("app:shortcut-settings-updated", listener);
    return () => ipcRenderer.removeListener("app:shortcut-settings-updated", listener);
  },
  onQuickCaptureHydrate: (callback) => {
    const listener = (_event, payload) => callback?.(payload);
    ipcRenderer.on("quick-capture:hydrate", listener);
    return () => ipcRenderer.removeListener("quick-capture:hydrate", listener);
  },
  onQuickCaptureSubmitted: (callback) => {
    const listener = (_event, payload) => callback?.(payload);
    ipcRenderer.on("quick-capture:submitted", listener);
    return () => ipcRenderer.removeListener("quick-capture:submitted", listener);
  },
  onThreadUpdated: (callback) => {
    const listener = (_event, payload) => callback?.(payload);
    ipcRenderer.on("app:thread-updated", listener);
    return () => ipcRenderer.removeListener("app:thread-updated", listener);
  },
  onWorkbenchUpdated: (callback) => {
    const listener = (_event, payload) => callback?.(payload);
    ipcRenderer.on("app:workbench-updated", listener);
    return () => ipcRenderer.removeListener("app:workbench-updated", listener);
  }
});
