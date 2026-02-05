import { Effects } from '../Effects'
import { PackageId } from '../osBindings'
import { deepEqual } from './deepEqual'
import { DropGenerator, DropPromise } from './Drop'
import { ServiceInterfaceFilled, filledAddress } from './getServiceInterface'

const makeManyInterfaceFilled = async ({
  effects,
  packageId,
  callback,
}: {
  effects: Effects
  packageId?: string
  callback?: () => void
}) => {
  const serviceInterfaceValues = await effects.listServiceInterfaces({
    packageId,
    callback,
  })

  const serviceInterfacesFilled: ServiceInterfaceFilled[] = await Promise.all(
    Object.values(serviceInterfaceValues).map(async (serviceInterfaceValue) => {
      const hostId = serviceInterfaceValue.addressInfo.hostId
      const host = await effects.getHostInfo({
        packageId,
        hostId,
        callback,
      })
      if (!host) {
        throw new Error(`host ${hostId} not found!`)
      }
      return {
        ...serviceInterfaceValue,
        host,
        addressInfo: filledAddress(host, serviceInterfaceValue.addressInfo),
      }
    }),
  )
  return serviceInterfacesFilled
}

export class GetServiceInterfaces<Mapped = ServiceInterfaceFilled[]> {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId?: string },
    readonly map: (interfaces: ServiceInterfaceFilled[]) => Mapped,
    readonly eq: (a: Mapped, b: Mapped) => boolean,
  ) {}

  /**
   * Returns the service interfaces for the package. Reruns the context from which it has been called if the underlying value changes
   */
  async const() {
    let abort = new AbortController()
    const watch = this.watch(abort.signal)
    const res = await watch.next()
    if (this.effects.constRetry) {
      watch
        .next()
        .then(() => {
          abort.abort()
          this.effects.constRetry && this.effects.constRetry()
        })
        .catch()
    }
    return res.value
  }
  /**
   * Returns the service interfaces for the package. Does nothing if the value changes
   */
  async once() {
    const { packageId } = this.opts
    const interfaceFilled: ServiceInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
      })

    return this.map(interfaceFilled)
  }

  private async *watchGen(abort?: AbortSignal) {
    let prev = null as { value: Mapped } | null
    const { packageId } = this.opts
    const resolveCell = { resolve: () => {} }
    this.effects.onLeaveContext(() => {
      resolveCell.resolve()
    })
    abort?.addEventListener('abort', () => resolveCell.resolve())
    while (this.effects.isInContext && !abort?.aborted) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
        resolveCell.resolve = resolve
      })
      const next = this.map(
        await makeManyInterfaceFilled({
          effects: this.effects,
          packageId,
          callback,
        }),
      )
      if (!prev || !this.eq(prev.value, next)) {
        yield next
      }
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new Error('aborted')))
  }

  /**
   * Watches the service interfaces for the package. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<Mapped, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the service interfaces for the package. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: Mapped | null,
      error?: Error,
    ) => { cancel: boolean } | Promise<{ cancel: boolean }>,
  ) {
    ;(async () => {
      const ctrl = new AbortController()
      for await (const value of this.watch(ctrl.signal)) {
        try {
          const res = await callback(value)
          if (res.cancel) {
            ctrl.abort()
            break
          }
        } catch (e) {
          console.error(
            'callback function threw an error @ GetServiceInterfaces.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetServiceInterfaces.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the service interfaces for the package. Returns when the predicate is true
   */
  waitFor(pred: (value: Mapped) => boolean): Promise<Mapped> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        for await (const next of this.watchGen(ctrl.signal)) {
          if (pred(next)) {
            return next
          }
        }
        throw new Error('context left before predicate passed')
      }),
      () => ctrl.abort(),
    )
  }
}

export function getOwnServiceInterfaces(effects: Effects): GetServiceInterfaces
export function getOwnServiceInterfaces<Mapped>(
  effects: Effects,
  map: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped>
export function getOwnServiceInterfaces<Mapped>(
  effects: Effects,
  map?: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped> {
  return new GetServiceInterfaces(
    effects,
    {},
    map ?? ((a) => a as Mapped),
    eq ?? ((a, b) => deepEqual(a, b)),
  )
}

export function getServiceInterfaces(
  effects: Effects,
  opts: { packageId: PackageId },
): GetServiceInterfaces
export function getServiceInterfaces<Mapped>(
  effects: Effects,
  opts: { packageId: PackageId },
  map: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped>
export function getServiceInterfaces<Mapped>(
  effects: Effects,
  opts: { packageId: PackageId },
  map?: (interfaces: ServiceInterfaceFilled[]) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceInterfaces<Mapped> {
  return new GetServiceInterfaces(
    effects,
    opts,
    map ?? ((a) => a as Mapped),
    eq ?? ((a, b) => deepEqual(a, b)),
  )
}
