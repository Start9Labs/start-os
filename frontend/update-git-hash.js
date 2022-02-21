// @ts-check
const fs = require('fs')
const childProcess = require('child_process')

const gitHash = String(childProcess.execSync('git rev-parse HEAD')).trim()

/**
 * @template T
 * @param {() => T} fn
 * @param {T} defaultValue
 * @returns
 */
function catchDefault(fn, defaultValue) {
  try {
    return fn()
  } catch {
    return defaultValue
  }
}

const origConfig = catchDefault(
  /** @returns {{gitHash?: string}} */
  () => JSON.parse(fs.readFileSync('./config.json')),
  {},
)
origConfig.gitHash = gitHash
fs.writeFileSync('./config.json', JSON.stringify(origConfig, null, 2))
