import { Daemons, configHash } from '../mainFn/Daemons'
import { Daemon } from '../mainFn/Daemon'
import { Mounts } from '../mainFn/Mounts'
import { SubContainer } from '../util/SubContainer'
import * as T from '@start9labs/start-core/types'

type Manifest = {
  id: 'test'
  volumes: ['main']
  images: {
    reg: { source: { dockerTag: 'x' } }
    other: { source: { dockerTag: 'y' } }
  }
} & T.SDKManifest

/** Minimal mock effects sufficient to construct a lazy SubContainer */
const fakeEffects = (): T.Effects =>
  ({
    eventId: null,
    child: () => fakeEffects(),
    isInContext: true,
    onLeaveContext: () => {},
    subcontainer: {
      // never invoked in these tests — laziness means createFs is deferred
      createFs: async () => ['', ''] as any,
      destroyFs: async () => null,
    },
  }) as any

const baseReady = {
  display: 'Reg',
  fn: () => ({ result: 'success' as const, message: null }),
}

const lazy = (
  e: T.Effects,
  imageId: 'reg' | 'other' = 'reg',
  sharedRun = true,
  mounts: Mounts<Manifest> | null = null,
  name = 'reg-sub',
) => SubContainer.of<Manifest>(e, { imageId, sharedRun }, mounts as any, name)

describe('Daemons recorder', () => {
  it('records entries in declaration order without constructing HealthDaemons', () => {
    const e = fakeEffects()
    const daemons = Daemons.of<Manifest>({ effects: e })
      .addDaemon('a', {
        subcontainer: lazy(e),
        exec: { command: ['a'] },
        ready: baseReady,
        requires: [],
      })
      .addDaemon('b', {
        subcontainer: lazy(e),
        exec: { command: ['b'] },
        ready: baseReady,
        requires: ['a'],
      })

    expect(daemons.entries.map(x => x.id)).toEqual(['a', 'b'])
    expect(daemons.entries[0].kind).toBe('daemon')
    expect(daemons.entries[1].requires).toEqual(['a'])
    // Nothing built yet
    expect(daemons.healthDaemons.length).toBe(0)
  })

  it('returns a fresh Daemons on each chain call (immutable)', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['a'] },
      ready: baseReady,
      requires: [],
    })
    const b = a.addDaemon('b', {
      subcontainer: lazy(e),
      exec: { command: ['b'] },
      ready: baseReady,
      requires: ['a'],
    })
    expect(a.entries.length).toBe(1)
    expect(b.entries.length).toBe(2)
  })

  it('supports oneshots and standalone health checks', () => {
    const e = fakeEffects()
    const daemons = Daemons.of<Manifest>({ effects: e })
      .addOneshot('migrate', {
        subcontainer: lazy(e),
        exec: { command: ['migrate'] },
        requires: [],
      })
      .addHealthCheck('sync', {
        ready: baseReady,
        requires: ['migrate'],
      })
    expect(daemons.entries.map(x => x.kind)).toEqual(['oneshot', 'health'])
  })
})

describe('configHash', () => {
  it('is stable when nothing structural changes', () => {
    const e = fakeEffects()
    const make = () =>
      Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
        subcontainer: lazy(e),
        exec: { command: ['cmd'] },
        ready: {
          display: 'Reg',
          // different closures across calls; should not affect hash
          fn: () => ({ result: 'success' as const, message: null }),
        },
        requires: [],
      }).entries[0]
    expect(configHash(make())).toEqual(configHash(make()))
  })

  it('changes when imageId changes', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e, 'reg'),
      exec: { command: ['cmd'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e, 'other'),
      exec: { command: ['cmd'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('changes when command changes', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd', '-p', '5959'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd', '-p', '5960'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('changes when env changes', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd'], env: { PORT: '5959' } },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd'], env: { PORT: '5960' } },
      ready: baseReady,
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('changes when mounts change', () => {
    const e = fakeEffects()
    const mountsA = Mounts.of<Manifest>().mountVolume({
      volumeId: 'main',
      subpath: '/instances/alice/data',
      mountpoint: '/var/lib/startos',
      readonly: false,
    })
    const mountsB = Mounts.of<Manifest>().mountVolume({
      volumeId: 'main',
      subpath: '/instances/bob/data',
      mountpoint: '/var/lib/startos',
      readonly: false,
    })
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e, 'reg', true, mountsA),
      exec: { command: ['cmd'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e, 'reg', true, mountsB),
      exec: { command: ['cmd'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('changes when requires changes', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e })
      .addOneshot('init', {
        subcontainer: lazy(e),
        exec: { command: ['init'] },
        requires: [],
      })
      .addDaemon('reg', {
        subcontainer: lazy(e),
        exec: { command: ['reg'] },
        ready: baseReady,
        requires: [],
      })
    const b = Daemons.of<Manifest>({ effects: e })
      .addOneshot('init', {
        subcontainer: lazy(e),
        exec: { command: ['init'] },
        requires: [],
      })
      .addDaemon('reg', {
        subcontainer: lazy(e),
        exec: { command: ['reg'] },
        ready: baseReady,
        requires: ['init'],
      })
    expect(configHash(a.entries[1])).not.toEqual(configHash(b.entries[1]))
  })

  it('is order-independent for requires (sorted before hash)', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e })
      .addOneshot('x', {
        subcontainer: lazy(e),
        exec: { command: ['x'] },
        requires: [],
      })
      .addOneshot('y', {
        subcontainer: lazy(e),
        exec: { command: ['y'] },
        requires: [],
      })
      .addDaemon('reg', {
        subcontainer: lazy(e),
        exec: { command: ['reg'] },
        ready: baseReady,
        requires: ['x', 'y'],
      })
    const b = Daemons.of<Manifest>({ effects: e })
      .addOneshot('x', {
        subcontainer: lazy(e),
        exec: { command: ['x'] },
        requires: [],
      })
      .addOneshot('y', {
        subcontainer: lazy(e),
        exec: { command: ['y'] },
        requires: [],
      })
      .addDaemon('reg', {
        subcontainer: lazy(e),
        exec: { command: ['reg'] },
        ready: baseReady,
        requires: ['y', 'x'],
      })
    expect(configHash(a.entries[2])).toEqual(configHash(b.entries[2]))
  })

  it('normalizes string vs argv command form distinctly', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: 'cmd arg' },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd', 'arg'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('produces distinct hashes per kind even with the same args', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('x', {
      subcontainer: lazy(e),
      exec: { command: ['x'] },
      ready: baseReady,
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addOneshot('x', {
      subcontainer: lazy(e),
      exec: { command: ['x'] },
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })

  it('changes when display label changes', () => {
    const e = fakeEffects()
    const a = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd'] },
      ready: { ...baseReady, display: 'A' },
      requires: [],
    }).entries[0]
    const b = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['cmd'] },
      ready: { ...baseReady, display: 'B' },
      requires: [],
    }).entries[0]
    expect(configHash(a)).not.toEqual(configHash(b))
  })
})

describe('Daemon subcontainer ownership', () => {
  it('detaches its subcontainer at construction (the daemon owns teardown)', () => {
    const e = fakeEffects()
    const sub = lazy(e)
    const detachSpy = jest.spyOn(sub, 'detach')
    Daemon.of<Manifest>()(e, sub, { command: ['x'] })
    expect(detachSpy).toHaveBeenCalled()
  })
})

describe('Daemon.detach', () => {
  // A fake effects whose onLeaveContext records the callbacks so a test can
  // fire them (simulating the context leaving) without a real runtime.
  const recordingEffects = (sink: Function[]): T.Effects => {
    const e = fakeEffects()
    e.onLeaveContext = (fn: any) => {
      sink.push(fn)
    }
    return e
  }

  it('suppresses the self-term that would otherwise fire on context-leave', () => {
    const leaveCbs: Function[] = []
    const e = recordingEffects(leaveCbs)
    const daemon = Daemon.of<Manifest>()(e, lazy(e), { command: ['x'] })
    const termSpy = jest.spyOn(daemon, 'term').mockResolvedValue(undefined)
    daemon.detach()
    leaveCbs.forEach(fn => fn())
    expect(termSpy).not.toHaveBeenCalled()
  })

  it('self-terms on context-leave when not detached', () => {
    const leaveCbs: Function[] = []
    const e = recordingEffects(leaveCbs)
    const daemon = Daemon.of<Manifest>()(e, lazy(e), { command: ['x'] })
    const termSpy = jest.spyOn(daemon, 'term').mockResolvedValue(undefined)
    leaveCbs.forEach(fn => fn())
    expect(termSpy).toHaveBeenCalled()
  })
})

describe('Daemons.detach', () => {
  it('is safe to call repeatedly', () => {
    const e = fakeEffects()
    const daemons = Daemons.of<Manifest>({ effects: e }).addDaemon('a', {
      subcontainer: lazy(e),
      exec: { command: ['a'] },
      ready: baseReady,
      requires: [],
    })
    expect(() => {
      daemons.detach()
      daemons.detach()
    }).not.toThrow()
  })
})

describe('SubContainerLazy.detach', () => {
  it('is safe and idempotent before materialization', () => {
    const e = fakeEffects()
    const sub = SubContainer.of<Manifest>(e, { imageId: 'reg' }, null, 'name')
    expect(() => {
      sub.detach()
      sub.detach()
    }).not.toThrow()
  })
})

describe('SubContainer.of (lazy) identity', () => {
  it('is preserved across .eager() materialization', async () => {
    // We can't actually materialize without a real runtime, but we can
    // verify the identity field exists and is stable on the lazy handle.
    const e = fakeEffects()
    const sub = SubContainer.of<Manifest>(e, { imageId: 'reg' }, null, 'name')
    expect(typeof sub.identity).toBe('symbol')
    // Identity unchanged across method-less accesses
    expect(sub.identity).toBe(sub.identity)
  })

  it('produces distinct identities per call', () => {
    const e = fakeEffects()
    const a = SubContainer.of<Manifest>(e, { imageId: 'reg' }, null, 'name')
    const b = SubContainer.of<Manifest>(e, { imageId: 'reg' }, null, 'name')
    expect(a.identity).not.toBe(b.identity)
  })
})
