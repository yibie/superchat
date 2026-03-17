export function createMainWindow({ BrowserWindow, shellState, preloadPath, onClosed }) {
  console.error("[windowFactory:main] create", {
    title: shellState.windows.main.title,
    htmlPath: shellState.windows.main.htmlPath,
    url: shellState.windows.main.url ?? null,
    preloadPath
  });
  const window = new BrowserWindow({
    title: shellState.windows.main.title,
    width: shellState.windows.main.width,
    height: shellState.windows.main.height,
    minWidth: 980,
    minHeight: 700,
    show: false,
    backgroundColor: "#f3efe6",
    webPreferences: {
      preload: preloadPath,
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false,
      webSecurity: false
    }
  });

  window.once("ready-to-show", () => {
    console.error("[windowFactory:main] ready-to-show");
    window.show();
    if (process.env.THREADNOTE_OPEN_DEVTOOLS === "1") {
      console.error("[windowFactory:main] opening devtools");
      window.webContents.openDevTools({ mode: "detach" });
    }
  });
  window.webContents.once("dom-ready", () => {
    console.error("[windowFactory:main] dom-ready", {
      url: window.webContents.getURL(),
      title: window.getTitle(),
      visible: window.isVisible()
    });
  });
  window.on("closed", () => onClosed?.());
  loadWindowSurface(window, shellState.windows.main, { surface: "main" }, "main");
  setTimeout(() => {
    if (!window.isDestroyed() && !window.isVisible()) {
      console.error("[windowFactory:main] still hidden after 3000ms", {
        url: window.webContents.getURL(),
        isLoading: window.webContents.isLoadingMainFrame()
      });
    }
  }, 3000);
  return window;
}

export function createQuickCaptureWindow({
  BrowserWindow,
  shellState,
  preloadPath,
  onClosed,
  onCloseRequest
}) {
  const window = new BrowserWindow({
    title: "Quick Capture",
    width: 520,
    height: 420,
    minWidth: 420,
    minHeight: 320,
    show: false,
    resizable: true,
    maximizable: false,
    fullscreenable: false,
    alwaysOnTop: true,
    backgroundColor: "#f3efe6",
    titleBarStyle: "hiddenInset",
    webPreferences: {
      preload: preloadPath,
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false,
      webSecurity: false
    }
  });

  window.on("close", (event) => onCloseRequest?.(event));
  window.on("closed", () => onClosed?.());
  window.once("ready-to-show", () => window.show());
  loadWindowSurface(window, shellState.windows.quickCapture, { surface: "quickCapture" }, "quickCapture");
  return window;
}

export function createSettingsWindow({
  BrowserWindow,
  shellState,
  preloadPath,
  onClosed
}) {
  const window = new BrowserWindow({
    title: "Settings",
    width: 760,
    height: 820,
    minWidth: 640,
    minHeight: 680,
    show: false,
    resizable: true,
    maximizable: false,
    fullscreenable: false,
    backgroundColor: "#f3efe6",
    webPreferences: {
      preload: preloadPath,
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false,
      webSecurity: false
    }
  });

  window.once("ready-to-show", () => window.show());
  window.on("closed", () => onClosed?.());
  loadWindowSurface(window, shellState.windows.settings, { surface: "settings" }, "settings");
  return window;
}

function loadWindowSurface(window, surface, query, label) {
  if (surface.url) {
    const url = new URL(surface.url);
    for (const [key, value] of Object.entries(query ?? {})) {
      if (value == null) continue;
      url.searchParams.set(key, String(value));
    }
    console.error(`[windowFactory:${label}] loadURL:start`, url.toString());
    window.loadURL(url.toString());
    console.error(`[windowFactory:${label}] loadURL:dispatched`);
    return;
  }

  console.error(`[windowFactory:${label}] loadFile:start`);
  window.loadFile(surface.htmlPath, { query });
  console.error(`[windowFactory:${label}] loadFile:dispatched`);
}
