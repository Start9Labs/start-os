import typescript from "@rollup/plugin-typescript"
import { nodeResolve } from "@rollup/plugin-node-resolve"
import swc from "@rollup/plugin-swc"
import auto from "@rollup/plugin-auto-install"

export default {
  input: "initSrc/index.ts",
  output: {
    file: "bundle.js",
    format: "cjs",
  },
  plugins: [nodeResolve(), swc()],
}
