// @ts-check
const fs = require('fs')
const childProcess = require('child_process')
const { env } = require('process')

const gitHash = String(
  childProcess.execSync('git describe --always --abbrev=40 --dirty=-modified'),
).trim()

const origConfig = require('./config.json')

const registries = require('./registries.json')

origConfig['gitHash'] = gitHash
if (/(^|-)beta($|-)/.test(env['ENVIRONMENT'] || '')) {
  origConfig.ui['marketplace'] = registries.beta
} else {
  origConfig.ui['marketplace'] = registries.prod
}
fs.writeFileSync('./config.json', JSON.stringify(origConfig, null, 2))
