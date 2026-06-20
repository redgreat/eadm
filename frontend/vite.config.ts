import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vite";
import solid from "vite-plugin-solid";

export default defineConfig({
  base: "/app/",
  plugins: [solid(), tailwindcss()],
  server: {
    port: 5173,
    proxy: {
      "/api": {
        target: "http://127.0.0.1:8080",
        changeOrigin: true
      },
      "/login": {
        target: "http://127.0.0.1:8080",
        changeOrigin: true
      },
      "/logout": {
        target: "http://127.0.0.1:8080",
        changeOrigin: true
      }
    }
  },
  build: {
    outDir: "dist",
    emptyOutDir: true
  }
});
