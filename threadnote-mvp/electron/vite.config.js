import path from "node:path";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  root: path.resolve(__dirname, "renderer"),
  base: "./",
  plugins: [react(), tailwindcss()],
  test: {
    environment: "jsdom",
    globals: true,
    setupFiles: [path.resolve(__dirname, "tests", "renderer", "setup.js")],
    include: [path.resolve(__dirname, "tests", "renderer", "**", "*.{test,spec}.{js,jsx}")]
  },
  build: {
    outDir: path.resolve(__dirname, "renderer-dist"),
    emptyOutDir: true,
    rollupOptions: {
      input: {
        main: path.resolve(__dirname, "renderer/index.html")
      }
    }
  }
});
