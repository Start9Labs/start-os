// @ts-check
const fs = require('fs')
const path = require('path')
const childProcess = require('child_process')

const gitHash = String(
  childProcess.execSync('git describe --always --abbrev=40 --dirty=-modified'),
).trim()

// Resolve the canonical repo-root config.json from __dirname (not cwd) — the
// apps and the Makefile build (update-config.sh) read/write it there.
const configPath = path.join(__dirname, '../../config.json')
const origConfig = require(configPath)

origConfig['gitHash'] = gitHash
fs.writeFileSync(configPath, JSON.stringify(origConfig, null, 2))
