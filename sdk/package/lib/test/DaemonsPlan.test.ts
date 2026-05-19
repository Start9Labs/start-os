import { DaemonsPlan, configHash } from '../mainFn/DaemonsPlan'
import { Mounts } from '../mainFn/Mounts'
import * as T from '../../../base/lib/types'

type Manifest = {
  id: 'test'
  volumes: ['main']
  images: { reg: { source: { dockerTag: 'x' } } }
} & T.SDKManifest

const baseSpec = {
  imageId: 'reg' as const,
  sharedRun: true,
  name: 'reg-sub',
}

const baseReady = {
  display: 'Reg',
  fn: () => ({ result: 'success' as const, message: null }),
}

describe('DaemonsPlan', () => {
  describe('builder', () => {
    it('records entries in declaration order', () => {
      const plan = DaemonsPlan.of<Manifest>()
        .addDaemon('a', {
          subcontainerSpec: baseSpec,
          exec: { command: ['a'] },
          ready: baseReady,
          requires: [],
        })
        .addDaemon('b', {
          subcontainerSpec: baseSpec,
          exec: { command: ['b'] },
          ready: baseReady,
          requires: ['a'],
        })

      expect(plan.entries.map((e) => e.id)).toEqual(['a', 'b'])
      expect(plan.entries[0].kind).toEqual('daemon')
      expect(plan.entries[1].requires).toEqual(['a'])
    })

    it('supports oneshots and standalone health checks', () => {
      const plan = DaemonsPlan.of<Manifest>()
        .addOneshot('migrate', {
          subcontainerSpec: baseSpec,
          exec: { command: ['migrate'] },
          requires: [],
        })
        .addHealthCheck('sync', {
          ready: baseReady,
          requires: ['migrate'],
        })

      expect(plan.entries.map((e) => e.kind)).toEqual(['oneshot', 'health'])
    })

    it('is immutable — chaining returns a new instance', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['a'] },
        ready: baseReady,
        requires: [],
      })
      const b = a.addDaemon('b', {
        subcontainerSpec: baseSpec,
        exec: { command: ['b'] },
        ready: baseReady,
        requires: ['a'],
      })
      expect(a.entries.length).toBe(1)
      expect(b.entries.length).toBe(2)
    })
  })

  describe('configHash', () => {
    it('is stable when nothing structural changes', () => {
      const plan1 = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec },
        exec: { command: ['cmd'] },
        ready: { ...baseReady },
        requires: [],
      })
      const plan2 = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec },
        exec: { command: ['cmd'] },
        // a different closure, same display+grace — should not affect hash
        ready: { display: 'Reg', fn: () => ({ result: 'failure', message: 'x' }) },
        requires: [],
      })

      expect(configHash(plan1.entries[0])).toEqual(configHash(plan2.entries[0]))
    })

    it('changes when the subcontainer image id changes', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec, imageId: 'reg' },
        exec: { command: ['cmd'] },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<{
        id: 'test'
        volumes: ['main']
        images: { other: { source: { dockerTag: 'y' } } }
      } & T.SDKManifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec, imageId: 'other' as any },
        exec: { command: ['cmd'] },
        ready: baseReady,
        requires: [],
      })

      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0] as any))
    })

    it('changes when the exec command changes', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd', '-p', '5959'] },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd', '-p', '5960'] },
        ready: baseReady,
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })

    it('changes when env changes', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd'], env: { PORT: '5959' } },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd'], env: { PORT: '5960' } },
        ready: baseReady,
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })

    it('changes when mounts change', () => {
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
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec, mounts: mountsA },
        exec: { command: ['cmd'] },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: { ...baseSpec, mounts: mountsB },
        exec: { command: ['cmd'] },
        ready: baseReady,
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })

    it('changes when requires changes', () => {
      const a = DaemonsPlan.of<Manifest>()
        .addOneshot('init', {
          subcontainerSpec: baseSpec,
          exec: { command: ['init'] },
          requires: [],
        })
        .addDaemon('reg', {
          subcontainerSpec: baseSpec,
          exec: { command: ['reg'] },
          ready: baseReady,
          requires: [],
        })
      const b = DaemonsPlan.of<Manifest>()
        .addOneshot('init', {
          subcontainerSpec: baseSpec,
          exec: { command: ['init'] },
          requires: [],
        })
        .addDaemon('reg', {
          subcontainerSpec: baseSpec,
          exec: { command: ['reg'] },
          ready: baseReady,
          requires: ['init'],
        })
      expect(configHash(a.entries[1])).not.toEqual(configHash(b.entries[1]))
    })

    it('changes when display label changes', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd'] },
        ready: { ...baseReady, display: 'A' },
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd'] },
        ready: { ...baseReady, display: 'B' },
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })

    it('is order-independent for requires (sorted before hash)', () => {
      const a = DaemonsPlan.of<Manifest>()
        .addOneshot('x', {
          subcontainerSpec: baseSpec,
          exec: { command: ['x'] },
          requires: [],
        })
        .addOneshot('y', {
          subcontainerSpec: baseSpec,
          exec: { command: ['y'] },
          requires: [],
        })
        .addDaemon('reg', {
          subcontainerSpec: baseSpec,
          exec: { command: ['reg'] },
          ready: baseReady,
          requires: ['x', 'y'],
        })
      const b = DaemonsPlan.of<Manifest>()
        .addOneshot('x', {
          subcontainerSpec: baseSpec,
          exec: { command: ['x'] },
          requires: [],
        })
        .addOneshot('y', {
          subcontainerSpec: baseSpec,
          exec: { command: ['y'] },
          requires: [],
        })
        .addDaemon('reg', {
          subcontainerSpec: baseSpec,
          exec: { command: ['reg'] },
          ready: baseReady,
          requires: ['y', 'x'],
        })

      expect(configHash(a.entries[2])).toEqual(configHash(b.entries[2]))
    })

    it('normalizes string vs argv command form differently', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: 'cmd arg' },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addDaemon('a', {
        subcontainerSpec: baseSpec,
        exec: { command: ['cmd', 'arg'] },
        ready: baseReady,
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })

    it('produces distinct hashes per kind even with the same args', () => {
      const a = DaemonsPlan.of<Manifest>().addDaemon('x', {
        subcontainerSpec: baseSpec,
        exec: { command: ['x'] },
        ready: baseReady,
        requires: [],
      })
      const b = DaemonsPlan.of<Manifest>().addOneshot('x', {
        subcontainerSpec: baseSpec,
        exec: { command: ['x'] },
        requires: [],
      })
      expect(configHash(a.entries[0])).not.toEqual(configHash(b.entries[0]))
    })
  })
})
