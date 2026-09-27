import js from "@eslint/js";
import solid from "eslint-plugin-solid/configs/typescript";
import tseslint from "typescript-eslint";

export default tseslint.config(
  { ignores: ["dist/**", "coverage/**", "node_modules/**"] },
  js.configs.recommended,
  ...tseslint.configs.recommended,
  {
    files: ["**/*.{ts,tsx}"],
    plugins: { solid: solid.plugins.solid },
    rules: {
      ...solid.rules,
      "@typescript-eslint/no-explicit-any": "error"
    }
  }
);
