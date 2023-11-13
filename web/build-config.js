// @ts-check
const fs = require('fs')
const childProcess = require('child_process')

const gitHash = String(
  childProcess.execSync('git describe --always --abbrev=40 --dirty=-modified'),
).trim()

const origConfig = require('./config.json')

origConfig['gitHash'] = gitHash
fs.writeFileSync('./config.json', JSON.stringify(origConfig, null, 2))
