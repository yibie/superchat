import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rendererRoot = path.resolve(__dirname, "../../../renderer-dist");
const rendererDevServerURL = process.env.THREADNOTE_RENDERER_DEV_URL ?? null;

export function bootstrapDesktopApp({ workspaceManager }) {
  return {
    status: "ready",
    workspace: workspaceManager?.describe?.() ?? null,
    windows: {
      main: {
        id: "main",
        title: "Threadnote",
        width: 1280,
        height: 860,
        htmlPath: path.join(rendererRoot, "index.html"),
        url: rendererDevServerURL ? buildDevServerURL(rendererDevServerURL, "/index.html") : null
      }
    }
  };
}

export function buildRendererURL(htmlPath, query = {}) {
  const url = new URL(`file://${htmlPath}`);
  for (const [key, value] of Object.entries(query)) {
    if (value == null) {
      continue;
    }
    url.searchParams.set(key, String(value));
  }
  return url.toString();
}

function buildDevServerURL(baseURL, pathname) {
  const url = new URL(baseURL);
  url.pathname = pathname;
  url.search = "";
  return url.toString();
}
