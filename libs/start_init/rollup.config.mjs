import typescript from "@rollup/plugin-typescript"

export default {
  input: "initSrc/index.ts",
  output: {
    file: "bundle.js",
    format: "cjs",
  },
  plugins: [typescript()],
}
