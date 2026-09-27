import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    include: ["src/**/*.test.ts"],
    coverage: {
      provider: "v8",
      reporter: ["text", "json-summary", "lcov"],
      include: ["src/lib/api/client.ts", "src/lib/cn.ts", "src/lib/datetime.ts"],
      exclude: ["src/lib/api/{auth,crontabs,dashboard,devices,finance,health,location,roles,system,users}.ts"],
      thresholds: {
        lines: 85,
        functions: 85,
        statements: 85,
        branches: 75
      }
    }
  }
});
