import { ServiceInterfaceBuilder } from '../../../base/lib/interfaces/ServiceInterfaceBuilder'
import { Effects } from '../../../base/lib/Effects'
import { MAX_BIND_PORT_RANGE_SIZE } from '../../../base/lib/interfaces/Host'
import { sdk } from '../test/output.sdk'

describe('host', () => {
  test('Testing that the types work', () => {
    async function test(effects: Effects) {
      const foo = sdk.MultiHost.of(effects, 'foo')
      const fooOrigin = await foo.bindPort(80, {
        protocol: 'http' as const,
        preferredExternalPort: 80,
      })
      const fooInterface = new ServiceInterfaceBuilder({
        effects,
        name: 'Foo',
        id: 'foo',
        description: 'A Foo',
        type: 'ui',
        username: 'bar',
        path: '/baz',
        query: { qux: 'yes' },
        schemeOverride: null,
        masked: false,
      })

      await fooOrigin.export([fooInterface])
    }
  })

  describe('MultiHost.bindPortRange', () => {
    const fakeEffects = (
      bindRange: jest.Mock = jest.fn(async () => null),
    ): Effects => ({ bindRange }) as unknown as Effects

    test('forwards the call to effects.bindRange with normalised params', async () => {
      const bindRange = jest.fn(async () => null)
      const host = sdk.MultiHost.of(fakeEffects(bindRange), 'turn')
      await host.bindPortRange({
        internalStartPort: 49152,
        externalStartPort: 49152,
        numberOfPorts: 100,
      })
      expect(bindRange).toHaveBeenCalledWith({
        id: 'turn',
        internalStartPort: 49152,
        externalStartPort: 49152,
        numberOfPorts: 100,
      })
    })

    test('accepts an offset range (internal !== external)', async () => {
      const bindRange = jest.fn(async () => null)
      const host = sdk.MultiHost.of(fakeEffects(bindRange), 'turn')
      await host.bindPortRange({
        internalStartPort: 49152,
        externalStartPort: 50000,
        numberOfPorts: 10,
      })
      expect(bindRange).toHaveBeenCalledWith({
        id: 'turn',
        internalStartPort: 49152,
        externalStartPort: 50000,
        numberOfPorts: 10,
      })
    })

    test('rejects numberOfPorts below 1', async () => {
      const host = sdk.MultiHost.of(fakeEffects(), 'turn')
      await expect(
        host.bindPortRange({
          internalStartPort: 49152,
          externalStartPort: 49152,
          numberOfPorts: 0,
        }),
      ).rejects.toThrow(/positive integer/)
    })

    test('rejects numberOfPorts past the SDK cap', async () => {
      const host = sdk.MultiHost.of(fakeEffects(), 'turn')
      await expect(
        host.bindPortRange({
          internalStartPort: 49152,
          externalStartPort: 49152,
          numberOfPorts: MAX_BIND_PORT_RANGE_SIZE + 1,
        }),
      ).rejects.toThrow(/exceeds maximum/)
    })

    test('rejects ranges that walk past port 65535', async () => {
      const host = sdk.MultiHost.of(fakeEffects(), 'turn')
      await expect(
        host.bindPortRange({
          internalStartPort: 65500,
          externalStartPort: 65500,
          numberOfPorts: 100,
        }),
      ).rejects.toThrow(/out of bounds/)
    })

    test('rejects privileged/system start ports (<= 1024)', async () => {
      const host = sdk.MultiHost.of(fakeEffects(), 'turn')
      await expect(
        host.bindPortRange({
          internalStartPort: 1024,
          externalStartPort: 1024,
          numberOfPorts: 10,
        }),
      ).rejects.toThrow(/greater than 1024/)
    })
  })
})
