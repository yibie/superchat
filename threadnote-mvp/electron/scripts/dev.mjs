import { spawn } from "node:child_process";
import process from "node:process";

const devServerURL = process.env.THREADNOTE_RENDERER_DEV_URL || "http://127.0.0.1:5173";
const viteArgs = ["vite", "--host", "127.0.0.1", "--port", "5173"];
const electronArgs = ["electron", "."];

let viteProcess = null;
let electronProcess = null;
let shuttingDown = false;

start().catch((error) => {
  console.error("[dev] failed", error);
  shutdown(1);
});

async function start() {
  viteProcess = spawn("npx", viteArgs, {
    stdio: "inherit",
    env: process.env
  });
  viteProcess.on("exit", (code, signal) => {
    if (shuttingDown) {
      return;
    }
    console.error("[dev] vite exited", { code, signal });
    shutdown(code ?? 1);
  });

  await waitForServer(devServerURL);

  electronProcess = spawn("npx", electronArgs, {
    stdio: "inherit",
    env: {
      ...process.env,
      THREADNOTE_RENDERER_DEV_URL: devServerURL
    }
  });
  electronProcess.on("exit", (code, signal) => {
    if (shuttingDown) {
      return;
    }
    console.error("[dev] electron exited", { code, signal });
    shutdown(code ?? 0);
  });

  for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"]) {
    process.on(signal, () => shutdown(0));
  }
}

async function waitForServer(url, retries = 120) {
  for (let attempt = 0; attempt < retries; attempt += 1) {
    try {
      const response = await fetch(url, { method: "GET" });
      if (response.ok) {
        console.error("[dev] vite server ready", url);
        return;
      }
    } catch {}
    await sleep(250);
  }
  throw new Error(`Timed out waiting for Vite server at ${url}`);
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function shutdown(code) {
  if (shuttingDown) {
    return;
  }
  shuttingDown = true;
  for (const child of [electronProcess, viteProcess]) {
    if (!child || child.killed) {
      continue;
    }
    child.kill("SIGTERM");
  }
  setTimeout(() => process.exit(code), 50);
}
