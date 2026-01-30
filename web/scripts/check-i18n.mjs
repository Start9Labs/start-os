import { readFileSync, readdirSync, statSync } from 'fs'
import { join, relative } from 'path'
import { fileURLToPath } from 'url'
import { dirname } from 'path'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const root = join(__dirname, '..')

// Extract dictionary keys from en.ts
const enPath = join(
  root,
  'projects/shared/src/i18n/dictionaries/en.ts',
)
const enSource = readFileSync(enPath, 'utf-8')
const validKeys = new Set()

for (const match of enSource.matchAll(/^\s+'(.+?)':\s*\d+/gm)) {
  validKeys.add(match[1])
}

if (validKeys.size === 0) {
  console.error('ERROR: Could not parse any keys from en.ts')
  process.exit(1)
}

console.log(`Loaded ${validKeys.size} i18n keys from en.ts`)

// Collect all .ts and .html files under projects/
function walk(dir, files = []) {
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry)
    if (entry === 'node_modules' || entry === 'dist') continue
    const stat = statSync(full)
    if (stat.isDirectory()) {
      walk(full, files)
    } else if (full.endsWith('.ts') || full.endsWith('.html')) {
      files.push(full)
    }
  }
  return files
}

const projectsDir = join(root, 'projects')
const files = walk(projectsDir)

const errors = []

for (const file of files) {
  // Skip the dictionary files themselves
  if (file.includes('/i18n/dictionaries/')) continue

  const source = readFileSync(file, 'utf-8')
  const lines = source.split('\n')

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]

    // Pattern 1: i18n.transform('Key') or i18n.transform("Key")
    for (const m of line.matchAll(/i18n\.transform\(\s*'([^']+)'\s*\)/g)) {
      if (!validKeys.has(m[1])) {
        errors.push({ file, line: i + 1, key: m[1] })
      }
    }
    for (const m of line.matchAll(/i18n\.transform\(\s*"([^"]+)"\s*\)/g)) {
      if (!validKeys.has(m[1])) {
        errors.push({ file, line: i + 1, key: m[1] })
      }
    }

    // Pattern 2: 'Key' | i18n or "Key" | i18n (Angular templates)
    for (const m of line.matchAll(/'([^']+)'\s*\|\s*i18n/g)) {
      if (!validKeys.has(m[1])) {
        errors.push({ file, line: i + 1, key: m[1] })
      }
    }
    for (const m of line.matchAll(/"([^"]+)"\s*\|\s*i18n/g)) {
      if (!validKeys.has(m[1])) {
        errors.push({ file, line: i + 1, key: m[1] })
      }
    }
  }
}

if (errors.length > 0) {
  console.error(`\nFound ${errors.length} invalid i18n key(s):\n`)
  for (const { file, line, key } of errors) {
    const rel = relative(root, file)
    console.error(`  ${rel}:${line}  "${key}"`)
  }
  console.error()
  process.exit(1)
} else {
  console.log('All i18n keys are valid.')
}
