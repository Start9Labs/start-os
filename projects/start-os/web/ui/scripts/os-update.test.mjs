import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { createPublicKey, generateKeyPairSync } from 'node:crypto'
import { readFileSync, existsSync } from 'node:fs'
import { registerHooks } from 'node:module'
import { resolve } from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'
import test from 'node:test'
import ts from 'typescript'
import '@angular/compiler'
import { createEnvironmentInjector, runInInjectionContext } from '@angular/core'
import { BehaviorSubject, EMPTY, Subject } from 'rxjs'

// Presentation substitutes keep browser-only dependencies out of class-method tests.
const boundaries = new Map()
function boundary(specifier, names, extra = '') {
  boundaries.set(
    specifier,
    names.map(name => `export class ${name} {}`).join('\n') + '\n' + extra,
  )
}
boundary(
  '@start9labs/shared',
  [
    'TaskService',
    'DialogService',
    'i18nPipe',
    'i18nService',
    'MarkdownPipe',
    'SafeLinksDirective',
  ],
  'export const LANGUAGES = []; export const LANGUAGE_TO_CODE = {}; export const getKeyboardName = () => ""; export const getAllKeyboardsSorted = () => [];',
)
boundary(
  '@taiga-ui/core',
  [
    'TuiButton',
    'TuiScrollbar',
    'TuiAppearance',
    'TuiCell',
    'TuiIcon',
    'TuiInput',
    'TuiTitle',
  ],
  'export const tuiCellOptionsProvider = () => [];',
)
boundary('@taiga-ui/kit', [
  'TuiBadge',
  'TuiBadgeNotification',
  'TuiButtonLoading',
  'TuiButtonSelect',
  'TuiDataListWrapper',
])
boundary('@taiga-ui/cdk', ['TuiAnimated'])
boundary('@taiga-ui/dompurify', ['NgDompurifyPipe'])
boundary(
  '@ng-web-apis/common',
  [],
  'export const WA_WINDOW = Symbol("window");',
)
boundary(
  '@taiga-ui/polymorpheus',
  [],
  `
import { inject, InjectionToken } from '@angular/core';
export const CONTEXT = new InjectionToken('dialog context');
export const injectContext = () => inject(CONTEXT);
export class PolymorpheusComponent { constructor(component) { this.component = component; } }
`,
)
boundary('src/app/services/api/embassy-api.service', ['ApiService'])
boundary('src/app/services/config.service', ['ConfigService'])
boundary('src/app/services/title.service', ['TitleDirective'])
boundary(
  'src/app/routes/portal/components/header/about.component',
  [],
  'export const ABOUT = {};',
)
for (const [file, name] of [
  ['keyboard-select.component', 'KeyboardSelectComponent'],
  ['server-name.dialog', 'ServerNameDialog'],
  ['snake.directive', 'SnakeDirective'],
]) {
  boundary(`./${file}`, [name])
}
const root = fileURLToPath(new URL('../../../../../', import.meta.url))
const ui = resolve(root, 'projects/start-os/web/ui')
const options = ts.readConfigFile(
  resolve(root, 'tsconfig.json'),
  ts.sys.readFile,
).config.compilerOptions
const compilerOptions = ts.convertCompilerOptionsFromJson(options, root).options
registerHooks({
  resolve(specifier, context, next) {
    if (boundaries.has(specifier))
      return { url: `test-boundary:${specifier}`, shortCircuit: true }
    let file
    if (specifier.startsWith('src/')) file = resolve(ui, specifier)
    else if (specifier.startsWith('.') && context.parentURL?.endsWith('.ts'))
      file = fileURLToPath(new URL(specifier, context.parentURL))
    if (file) {
      for (const candidate of [file, `${file}.ts`, `${file}/index.ts`]) {
        if (existsSync(candidate))
          return { url: pathToFileURL(candidate).href, shortCircuit: true }
      }
    }
    return next(
      specifier,
      context.parentURL?.startsWith('test-boundary:')
        ? { ...context, parentURL: import.meta.url }
        : context,
    )
  },
  load(url, context, next) {
    if (url.startsWith('test-boundary:'))
      return {
        format: 'module',
        source: boundaries.get(url.slice('test-boundary:'.length)),
        shortCircuit: true,
      }
    if (url.endsWith('.ts'))
      return {
        format: 'module',
        source: ts.transpileModule(readFileSync(new URL(url), 'utf8'), {
          compilerOptions,
          fileName: fileURLToPath(url),
        }).outputText,
        shortCircuit: true,
      }
    return next(url, context)
  },
})
const { PatchDB } = await import('patch-db-client')
const { OSService } = await import('src/app/services/os.service')
const { ApiService } = await import('src/app/services/api/embassy-api.service')
const { TaskService, DialogService, i18nPipe, i18nService } =
  await import('@start9labs/shared')
const { CONTEXT } = await import('@taiga-ui/polymorpheus')
const { WA_WINDOW } = await import('@ng-web-apis/common')
const { ConfigService } = await import('src/app/services/config.service')
const { OS_UPDATE_SIGNERS, trustedOsUpdateSigners } =
  await import('src/app/utils/os-update-signers')
const { default: General } =
  await import('src/app/routes/portal/routes/system/routes/general/general.component')
const { SystemUpdateModal, UPDATE } =
  await import('src/app/routes/portal/routes/system/routes/general/update.component')

function deferred() {
  let resolve, reject
  const promise = new Promise((yes, no) => {
    resolve = yes
    reject = no
  })
  return { promise, resolve, reject }
}
const untrustedSigner = generateKeyPairSync('ed25519').publicKey.export({
  type: 'spki',
  format: 'pem',
})
const squashfs = {
  x86_64: { signatures: { [OS_UPDATE_SIGNERS[0]]: 'signature' } },
}
const catalog = {
  '0.4.0.2': { releaseNotes: 'equal', squashfs },
  '0.4.0.4': { releaseNotes: 'newest', squashfs },
  '0.4.0.1': { releaseNotes: 'older', squashfs },
  '0.4.0.3': { releaseNotes: 'newer', squashfs },
}
const candidates = [
  { version: '0.4.0.4', notes: 'newest', squashfs },
  { version: '0.4.0.3', notes: 'newer', squashfs },
]
const state = version => ({
  serverInfo: { id: 'server-id', version, statusInfo: {} },
  ui: { startosRegistry: 'https://registry.test', snakeHighScore: 0 },
})
const tick = () => new Promise(resolve => setImmediate(resolve))
function fixture(t, cachedVersion) {
  const source = new Subject()
  const cache = new BehaviorSubject(
    cachedVersion
      ? { id: 1, value: state(cachedVersion) }
      : { id: 0, value: {} },
  )
  const patch = new PatchDB(source, cache)
  patch.start()
  let revision = cachedVersion ? 1 : 0
  const requests = [],
    submissions = [],
    opened = [],
    alerts = [],
    tasks = [],
    confirmations = [],
    updateErrors = []
  let completed = 0
  const api = {
    rpc: {
      registry: {
        os: {
          version: {
            get(params) {
              const response = deferred()
              requests.push({ params, ...response })
              return response.promise
            },
          },
        },
      },
      server: {
        async deviceInfo() {
          return { os: { platform: 'x86_64' } }
        },
        async update(params) {
          submissions.push(params)
          const error = updateErrors.shift()
          if (error) throw error
        },
      },
    },
  }
  const taskService = {
    run(fn) {
      const task = (async () => {
        try {
          await fn()
          return true
        } catch {
          return false
        }
      })()
      tasks.push(task)
      return task
    },
  }
  const injector = createEnvironmentInjector([
    { provide: PatchDB, useValue: patch },
    { provide: ApiService, useValue: api },
    { provide: TaskService, useValue: taskService },
    {
      provide: DialogService,
      useValue: {
        openComponent(content) {
          opened.push(content)
          return EMPTY
        },
        openAlert(message) {
          alerts.push(message)
          return EMPTY
        },
        openConfirm(options) {
          const response = new Subject()
          confirmations.push({ options, response })
          return response
        },
      },
    },
    { provide: i18nPipe, useValue: {} },
    { provide: i18nService, useValue: {} },
    { provide: WA_WINDOW, useValue: {} },
    { provide: ConfigService, useValue: {} },
    {
      provide: CONTEXT,
      useValue: {
        $implicit: {
          complete() {
            completed++
          },
        },
      },
    },
    { provide: OSService, useFactory: () => new OSService() },
  ])
  const os = injector.get(OSService)
  const availability = [],
    shown = []
  const subs = [
    os.updateAvailable$.subscribe(value => availability.push(value)),
    os.showUpdate$.subscribe(value => shown.push(value)),
  ]
  t.after(() => {
    subs.forEach(sub => sub.unsubscribe())
    injector.destroy()
    patch.stop()
  })
  return {
    os,
    patch,
    api,
    requests,
    submissions,
    opened,
    alerts,
    tasks,
    confirmations,
    updateErrors,
    availability,
    shown,
    get completed() {
      return completed
    },
    make: Class => runInInjectionContext(injector, () => new Class()),
    live(version) {
      source.next([{ id: ++revision, value: state(version) }])
    },
    version(version) {
      source.next([
        {
          id: ++revision,
          patch: [
            { op: 'replace', path: '/serverInfo/version', value: version },
          ],
        },
      ])
    },
    async load(response = catalog) {
      const loading = os.loadOS()
      await tick()
      requests.at(-1).resolve(response)
      await loading
    },
  }
}

for (const liveBeforeResponse of [true, false]) {
  test(`cached older version is replaced by live current ${liveBeforeResponse ? 'before' : 'after'} registry response`, async t => {
    const f = fixture(t, '0.4.0.1')
    const loading = f.os.loadOS()
    await tick()
    assert.deepEqual(f.requests[0].params, {
      registry: 'https://registry.test',
      serverId: 'server-id',
    })
    if (liveBeforeResponse) f.live('0.4.0.2')
    f.requests[0].resolve({ '0.4.0.2': { releaseNotes: 'installed' } })
    await loading
    if (!liveBeforeResponse) {
      assert.equal(f.os.updateCandidates()[0].version, '0.4.0.2')
      f.live('0.4.0.2')
    }
    assert.deepEqual(f.os.updateCandidates(), [])
    assert.equal(f.availability.at(-1), false)
    assert.equal(f.shown.at(-1), false)
    assert.equal(f.requests.length, 1)
  })
}

test('without a cache, load waits for the first live state', async t => {
  const f = fixture(t)
  const loading = f.os.loadOS()
  await tick()
  assert.equal(f.requests.length, 0)
  assert.deepEqual(f.os.updateCandidates(), [])
  f.live('0.4.0.2')
  await tick()
  f.requests[0].resolve({ '0.4.0.2': { releaseNotes: 'installed' } })
  await loading
  assert.deepEqual(f.os.updateCandidates(), [])
  assert.equal(f.availability.at(-1), false)
})

test('an empty catalog clears availability and cannot be submitted', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  await f.load({})
  assert.deepEqual(f.os.updateCandidates(), [])
  assert.equal(f.availability.at(-1), false)
  f.make(General).update()
  await f.make(SystemUpdateModal).update()
  assert.deepEqual(f.opened, [])
  assert.deepEqual(f.submissions, [])
  assert.equal(f.tasks.length, 0)
})

test('strictly newer candidates sort descending; refresh failure preserves catalog; version updates do not refetch', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  assert.deepEqual(f.os.updateCandidates(), candidates)
  assert.equal(f.availability.at(-1), true)
  assert.equal(f.shown.at(-1), true)
  const refresh = f.os.loadOS()
  await tick()
  assert.equal(f.requests.length, 2)
  assert.deepEqual(f.os.updateCandidates(), candidates)
  f.requests[1].reject(new Error('registry unavailable'))
  await assert.rejects(refresh, /registry unavailable/)
  assert.deepEqual(f.os.updateCandidates(), candidates)
  f.version('0.4.0.3')
  assert.deepEqual(f.os.updateCandidates(), [candidates[0]])
  f.version('0.4.0.4')
  assert.deepEqual(f.os.updateCandidates(), [])
  assert.equal(f.requests.length, 2)
  await f.load({ '0.4.0.5': { releaseNotes: 'refreshed', squashfs } })
  assert.deepEqual(f.os.updateCandidates(), [
    { version: '0.4.0.5', notes: 'refreshed', squashfs },
  ])
})

test('General checks but does not open a modal without candidates', async t => {
  const f = fixture(t, '0.4.0.4')
  const general = f.make(General)
  general.onUpdate()
  await tick()
  f.requests[0].resolve(catalog)
  await Promise.all(f.tasks)
  await tick()
  assert.deepEqual(f.opened, [])
  assert.equal(f.alerts.length, 1)
  general.update()
  assert.deepEqual(f.opened, [])
  await f.make(SystemUpdateModal).update()
  assert.deepEqual(f.submissions, [])
  assert.equal(f.completed, 0)
})

test('General opens a modal after a successful manual check finds candidates', async t => {
  const f = fixture(t, '0.4.0.2')
  f.make(General).onUpdate()
  await tick()
  assert.deepEqual(f.opened, [])
  f.requests[0].resolve(catalog)
  await Promise.all(f.tasks)
  await tick()
  assert.deepEqual(f.opened, [UPDATE])
  assert.deepEqual(f.alerts, [])
})

test('General does not announce up-to-date after a failed manual check', async t => {
  const f = fixture(t, '0.4.0.2')
  f.make(General).onUpdate()
  await tick()
  f.requests[0].reject(new Error('registry unavailable'))
  await Promise.all(f.tasks)
  await tick()
  assert.deepEqual(f.opened, [])
  assert.deepEqual(f.alerts, [])
})

test('General opens the update modal and submission targets exactly the newest candidate', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  f.make(General).onUpdate()
  assert.deepEqual(f.opened, [UPDATE])
  assert.equal(f.requests.length, 1)
  await f.make(SystemUpdateModal).update()
  await Promise.all(f.tasks)
  assert.equal(f.confirmations.length, 0)
  assert.deepEqual(f.submissions, [
    {
      targetVersion: '=0.4.0.4',
      registry: 'https://registry.test',
      progress: false,
      acceptSigners: { any: [{ signer: OS_UPDATE_SIGNERS[0] }] },
    },
  ])
  assert.equal(f.completed, 1)
})

test('a candidate disappearing after the modal opens prevents submission', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  f.make(General).onUpdate()
  const modal = f.make(SystemUpdateModal)
  f.version('0.4.0.4')
  await modal.update()
  assert.equal(f.opened.length, 1)
  assert.deepEqual(f.submissions, [])
  assert.equal(f.completed, 0)
  assert.equal(f.tasks.length, 0)
})

for (const [name, key] of [
  ['drbonez', OS_UPDATE_SIGNERS[0]],
  ['matthill', OS_UPDATE_SIGNERS[1]],
  ['Start9', OS_UPDATE_SIGNERS[2]],
]) {
  test(`${name} listed as a signer skips the warning without verifying in the UI`, async t => {
    const f = fixture(t, '0.4.0.2')
    await f.load({
      '0.4.0.4': {
        releaseNotes: '',
        squashfs: {
          x86_64: {
            signatures: {
              [key]: 'left to downloader',
              [untrustedSigner]: 'another signature',
            },
          },
        },
      },
    })
    await f.make(SystemUpdateModal).update()
    await Promise.all(f.tasks)
    assert.equal(f.confirmations.length, 0)
    assert.equal(f.submissions.length, 1)
    assert.deepEqual(f.submissions[0].acceptSigners, { any: [{ signer: key }] })
    assert.equal(f.completed, 1)
  })
}

test('the trusted list contains the requested maintainer keys and the Start9 OpenPGP primary key', () => {
  for (const body of [
    'MCowBQYDK2VwAyEAgpcI3hOExv1njCt+zJoYBartdyk4SKiqSIy3+9tcpOI=',
    'MCowBQYDK2VwAyEAfeubYt/ozIfZspUZdmu2Fa6sK2psMnHVGxuVolo8u40=',
  ]) {
    assert.equal(
      trustedOsUpdateSigners([
        `-----BEGIN PUBLIC KEY-----\n${body}\n-----END PUBLIC KEY-----`,
      ]).length,
      1,
    )
  }
  const records = execFileSync(
    'gpg',
    [
      '--batch',
      '--no-options',
      '--with-colons',
      '--with-key-data',
      '--show-keys',
      resolve(root, 'apt/start9.gpg'),
    ],
    { encoding: 'utf8' },
  ).split('\n')
  assert.equal(
    records.find(line => line.startsWith('pub:')).split(':')[3],
    '22',
  )
  const point = records.find(line => line.startsWith('pkd:1:')).split(':')[3]
  assert.equal(point.slice(0, 2), '40')
  const key = createPublicKey({
    format: 'jwk',
    key: {
      kty: 'OKP',
      crv: 'Ed25519',
      x: Buffer.from(point.slice(2), 'hex').toString('base64url'),
    },
  }).export({ format: 'pem', type: 'spki' })
  assert.deepEqual(trustedOsUpdateSigners([key]), [key])
  assert.equal(OS_UPDATE_SIGNERS.length, 3)
  assert.deepEqual(
    trustedOsUpdateSigners([
      '-----BEGIN PUBLIC KEY-----\nMCowBQYDK2VwAyEAqPjOeMD2CB3UEASoICgfJlfrO5ygjOSBCCNRyDBs75A=\n-----END PUBLIC KEY-----\n',
      '-----BEGIN PUBLIC KEY-----\nMCowBQYDK2VwAyEASv7fB+rs6FRL+pk7KXaoXONNn26dRkwMB9LXUqmXXLM=\n-----END PUBLIC KEY-----\n',
    ]),
    [],
  )
})

test('trusted signer matching ignores PEM whitespace but rejects unrelated keys and unsigned metadata', () => {
  const key = OS_UPDATE_SIGNERS[0].replace(/\n/g, '\r\n')
  assert.deepEqual(trustedOsUpdateSigners([key]), [key])
  assert.deepEqual(trustedOsUpdateSigners(['unrelated key']), [])
  assert.deepEqual(trustedOsUpdateSigners([]), [])
})

for (const decision of [true, false, undefined]) {
  test(`an untrusted update warns before submission; confirmation = ${decision}`, async t => {
    const f = fixture(t, '0.4.0.2')
    await f.load({
      '0.4.0.4': {
        releaseNotes: '',
        squashfs: {
          x86_64: { signatures: { [untrustedSigner]: 'signature' } },
          aarch64: { signatures: { [OS_UPDATE_SIGNERS[0]]: 'signature' } },
        },
        iso: squashfs,
        authorized: ['drbonez'],
      },
    })
    await f.make(SystemUpdateModal).update()
    await tick()
    assert.equal(f.confirmations.length, 1)
    assert.deepEqual(f.submissions, [])
    assert.equal(f.completed, 0)
    const { response, options } = f.confirmations[0]
    assert.equal(options.label, 'Danger: untrusted OS update')
    if (decision !== undefined) response.next(decision)
    response.complete()
    await Promise.all(f.tasks)
    assert.equal(f.submissions.length, decision ? 1 : 0)
    if (decision) {
      assert.deepEqual(f.submissions[0].acceptSigners, {
        any: [{ signer: untrustedSigner }],
      })
    }
    assert.equal(f.completed, decision ? 1 : 0)
  })
}

for (const assets of [{ x86_64: { signatures: {} } }, {}]) {
  test(`missing signatures or platform metadata warns before submission: ${JSON.stringify(assets)}`, async t => {
    const f = fixture(t, '0.4.0.2')
    await f.load({ '0.4.0.4': { releaseNotes: '', squashfs: assets } })
    await f.make(SystemUpdateModal).update()
    await tick()
    assert.equal(f.confirmations.length, 1)
    assert.deepEqual(f.submissions, [])
    f.confirmations[0].response.next(false)
    f.confirmations[0].response.complete()
    await Promise.all(f.tasks)
  })
}

test('a candidate disappearing during confirmation prevents submission', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load({ '0.4.0.4': { releaseNotes: '', squashfs: {} } })
  await f.make(SystemUpdateModal).update()
  await tick()
  f.version('0.4.0.4')
  f.confirmations[0].response.next(true)
  f.confirmations[0].response.complete()
  await Promise.all(f.tasks)
  assert.deepEqual(f.submissions, [])
  assert.equal(f.completed, 0)
})

test('a candidate disappearing during device lookup prevents submission', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  const device = deferred()
  f.api.rpc.server.deviceInfo = () => device.promise
  await f.make(SystemUpdateModal).update()
  f.version('0.4.0.4')
  device.resolve({ os: { platform: 'x86_64' } })
  await Promise.all(f.tasks)
  assert.deepEqual(f.submissions, [])
  assert.equal(f.completed, 0)
})

test('device lookup and downloader failures stay in TaskService and leave the modal open', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  f.updateErrors.push(new Error('invalid signature'))
  await f.make(SystemUpdateModal).update()
  assert.deepEqual(await Promise.all(f.tasks), [false])
  assert.equal(f.confirmations.length, 0)
  assert.equal(f.completed, 0)

  f.api.rpc.server.deviceInfo = async () => {
    throw new Error('unavailable')
  }
  await f.make(SystemUpdateModal).update()
  assert.deepEqual(await Promise.all(f.tasks), [false, false])
  assert.equal(f.submissions.length, 1)
  assert.equal(f.completed, 0)
})

test('a candidate disappearing during the asynchronous UI registry lookup prevents submission', async t => {
  const f = fixture(t, '0.4.0.2')
  await f.load()
  const modal = f.make(SystemUpdateModal)
  const updating = modal.update()
  f.version('0.4.0.4')
  await updating
  assert.deepEqual(await Promise.all(f.tasks), [true])
  assert.deepEqual(f.submissions, [])
  assert.equal(f.completed, 0)
})
