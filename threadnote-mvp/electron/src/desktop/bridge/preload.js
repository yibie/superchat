import { contextBridge, ipcRenderer, webUtils } from "electron";

contextBridge.exposeInMainWorld("threadnoteDesktop", {
  getShellState: () => ipcRenderer.invoke("desktop:get-shell-state"),

  openSettings: () => ipcRenderer.invoke("desktop:open-settings"),
  closeWindow: () => ipcRenderer.invoke("desktop:close-window"),
  getWorkbenchState: () => ipcRenderer.invoke("app:get-workbench-state"),
  createWorkspace: () => ipcRenderer.invoke("app:create-workspace"),
  openWorkspace: () => ipcRenderer.invoke("app:open-workspace"),
  createThread: (payload) => ipcRenderer.invoke("app:create-thread", payload),
  createThreadFromEntry: (payload) => ipcRenderer.invoke("app:create-thread-from-entry", payload),
  openThread: (threadID) => ipcRenderer.invoke("app:open-thread", threadID),
  prepareThread: (payload) => ipcRenderer.invoke("app:prepare-thread", payload),
  pingAIProvider: () => ipcRenderer.invoke("app:ping-ai-provider"),
  getAIProviderConfig: () => ipcRenderer.invoke("app:get-ai-provider-config"),
  saveAIProviderConfig: (payload) => ipcRenderer.invoke("app:save-ai-provider-config", payload),
  saveAndPingAIProvider: (payload) => ipcRenderer.invoke("app:save-and-ping-ai-provider", payload),
  appendReply: (payload) => ipcRenderer.invoke("app:append-reply", payload),
  updateEntryText: (payload) => ipcRenderer.invoke("app:update-entry-text", payload),
  deleteEntry: (entryID) => ipcRenderer.invoke("app:delete-entry", entryID),
  routeEntryToThread: (payload) => ipcRenderer.invoke("app:route-entry-to-thread", payload),
  openLocator: (locator) => ipcRenderer.invoke("app:open-locator", locator),
  getEntryRichPreview: (entryID) => ipcRenderer.invoke("app:get-entry-rich-preview", entryID),
  submitCapture: (payload) => ipcRenderer.invoke("app:submit-capture", payload),
  copyAttachment: (filePath) => ipcRenderer.invoke("app:copy-attachment", filePath),
  writeAttachmentBuffer: (payload) => ipcRenderer.invoke("app:write-attachment-buffer", payload),
  getFilePath: (file) => webUtils.getPathForFile(file),
  onOpenSettings: (callback) => {
    ipcRenderer.removeAllListeners("desktop:open-settings");
    ipcRenderer.on("desktop:open-settings", () => callback?.());
  }
});
