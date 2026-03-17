export function buildAppMenuTemplate({
  onOpenSettings,
  onOpenQuickCapture,
  quickCaptureAccelerator = null,
  settingsAccelerator = null
}) {
  return [
    {
      label: "Threadnote",
      submenu: [
        {
          label: "Quick Capture",
          ...(quickCaptureAccelerator ? { accelerator: quickCaptureAccelerator } : {}),
          click: () => onOpenQuickCapture?.()
        },
        { type: "separator" },
        {
          label: "Settings...",
          ...(settingsAccelerator ? { accelerator: settingsAccelerator } : {}),
          click: () => onOpenSettings?.()
        },
        { type: "separator" },
        { role: "quit" }
      ]
    },
    {
      label: "Edit",
      submenu: [
        { role: "undo" },
        { role: "redo" },
        { type: "separator" },
        { role: "cut" },
        { role: "copy" },
        { role: "paste" },
        { role: "pasteAndMatchStyle" },
        { role: "delete" },
        { type: "separator" },
        { role: "selectAll" }
      ]
    },
    {
      label: "View",
      submenu: [
        { role: "reload" },
        { role: "forceReload" },
        { role: "toggleDevTools" },
        { type: "separator" },
        { role: "resetZoom" },
        { role: "zoomIn" },
        { role: "zoomOut" },
        { type: "separator" },
        { role: "togglefullscreen" }
      ]
    },
    {
      label: "Window",
      submenu: [{ role: "minimize" }, { role: "close" }]
    }
  ];
}
