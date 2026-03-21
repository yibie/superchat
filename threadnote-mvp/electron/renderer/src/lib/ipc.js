const api = typeof window !== "undefined" ? window.threadnoteDesktop : null;

function call(method, ...args) {
  if (!api?.[method]) {
    console.warn(`IPC method "${method}" not available`);
    return Promise.resolve(null);
  }
  return api[method](...args);
}

export const ipc = {
  getShellState: () => call("getShellState"),
  closeWindow: () => call("closeWindow"),
  getWorkbenchState: () => call("getWorkbenchState"),
  repairWorkspaceRelations: () => call("repairWorkspaceRelations"),
  getStreamPage: (payload) => call("getStreamPage", payload),
  getThreadPage: (payload) => call("getThreadPage", payload),
  getShortcutSettings: () => call("getShortcutSettings"),
  updateShortcutSetting: (payload) => call("updateShortcutSetting", payload),
  clearShortcutSetting: (actionId) => call("clearShortcutSetting", actionId),
  openSettingsWindow: () => call("openSettingsWindow"),
  openQuickCapture: (payload) => call("openQuickCapture", payload),
  closeQuickCapture: () => call("closeQuickCapture"),
  resizeQuickCapture: (payload) => call("resizeQuickCapture", payload),
  importFromClipboard: () => call("importFromClipboard"),
  submitQuickCapture: (payload) => call("submitQuickCapture", payload),
  createWorkspace: () => call("createWorkspace"),
  openWorkspace: () => call("openWorkspace"),
  submitCapture: (payload) => call("submitCapture", payload),
  appendReply: (payload) => call("appendReply", payload),
  updateEntryText: (payload) => call("updateEntryText", payload),
  updateEntryKind: (payload) => call("updateEntryKind", payload),
  updateEntryStatus: (payload) => call("updateEntryStatus", payload),
  deleteEntry: (entryID) => call("deleteEntry", entryID),
  routeEntryToThread: (payload) => call("routeEntryToThread", payload),
  createThread: (payload) => call("createThread", payload),
  createThreadFromEntry: (payload) => call("createThreadFromEntry", payload),
  archiveThread: (threadID) => call("archiveThread", threadID),
  updateThreadTitle: (payload) => call("updateThreadTitle", payload),
  openThread: (threadID) => call("openThread", threadID),
  prepareThread: (payload) => call("prepareThread", payload),
  getEntryRichPreview: (entryID) => call("getEntryRichPreview", entryID),
  getLocatorRichPreview: (locator) => call("getLocatorRichPreview", locator),
  getAIProviderConfig: () => call("getAIProviderConfig"),
  saveAIProviderConfig: (payload) => call("saveAIProviderConfig", payload),
  testAIProvider: (payload) => call("testAIProvider", payload),
  pingAIProvider: () => call("pingAIProvider"),
  openLocator: (locator) => call("openLocator", locator),
  copyAttachment: (filePath) => call("copyAttachment", filePath),
  writeAttachmentBuffer: (payload) => call("writeAttachmentBuffer", payload),
  getFilePath: (file) => api?.getFilePath?.(file) ?? null,
  onOpenSettings: (cb) => api?.onOpenSettings?.(cb),
  onShortcutSettingsUpdated: (cb) => api?.onShortcutSettingsUpdated?.(cb),
  onQuickCaptureHydrate: (cb) => api?.onQuickCaptureHydrate?.(cb),
  onQuickCaptureSubmitted: (cb) => api?.onQuickCaptureSubmitted?.(cb),
  onThreadUpdated: (cb) => api?.onThreadUpdated?.(cb),
  onWorkbenchUpdated: (cb) => api?.onWorkbenchUpdated?.(cb),
};
