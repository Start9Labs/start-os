#!/usr/bin/env node
// SDK-bundled lint runner. Invoked from a package dir (cwd = package) by
// s9pk.mk as part of the build gate: `node node_modules/@start9labs/start-sdk/lint.mjs`.
// Resolves eslint + the shared config from the SDK's own node_modules, so packages
// need no eslint devDep (which would clobber the npm-linked SDK symlink).
import { ESLint } from 'eslint'
import config from './eslint.config.base.mjs'

const eslint = new ESLint({
  cwd: process.cwd(),
  overrideConfigFile: true,
  overrideConfig: config,
  errorOnUnmatchedPattern: false,
})

const results = await eslint.lintFiles(['startos/**/*.ts'])
const formatter = await eslint.loadFormatter('stylish')
const output = await formatter.format(results)
if (output.trim()) console.error(output)

const errors = results.reduce((n, r) => n + r.errorCount, 0)
process.exit(errors > 0 ? 1 : 0)
