import { Effects } from '../../../base/lib/Effects'
import { Manifest, PackageId } from '../../../base/lib/osBindings'
import { DropGenerator, DropPromise } from '../../../base/lib/util/Drop'
import { deepEqual } from '../../../base/lib/util/deepEqual'

export class GetServiceManifest<Mapped = Manifest> {
  constructor(
    readonly effects: Effects,
    readonly packageId: PackageId,
    readonly map: (manifest: Manifest | null) => Mapped,
    readonly eq: (a: Mapped, b: Mapped) => boolean,
  ) {}

  /**
   * Returns the manifest of a service. Reruns the context from which it has been called if the underlying value changes
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
   * Returns the manifest of a service. Does nothing if it changes
   */
  async once() {
    const manifest = await this.effects.getServiceManifest({
      packageId: this.packageId,
    })
    return this.map(manifest)
  }

  private async *watchGen(abort?: AbortSignal) {
    let prev = null as { value: Mapped } | null
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
        await this.effects.getServiceManifest({
          packageId: this.packageId,
          callback: () => callback(),
        }),
      )
      if (!prev || !this.eq(prev.value, next)) {
        prev = { value: next }
        yield next
      }
      await waitForNext
    }
    return new Promise<never>((_, rej) => rej(new Error('aborted')))
  }

  /**
   * Watches the manifest of a service. Returns an async iterator that yields whenever the value changes
   */
  watch(abort?: AbortSignal): AsyncGenerator<Mapped, never, unknown> {
    const ctrl = new AbortController()
    abort?.addEventListener('abort', () => ctrl.abort())
    return DropGenerator.of(this.watchGen(ctrl.signal), () => ctrl.abort())
  }

  /**
   * Watches the manifest of a service. Takes a custom callback function to run whenever it changes
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
            'callback function threw an error @ GetServiceManifest.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ GetServiceManifest.onChange',
          e,
        ),
      )
  }

  /**
   * Watches the manifest of a service. Returns when the predicate is true
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

export function getServiceManifest(
  effects: Effects,
  packageId: PackageId,
): GetServiceManifest<Manifest>
export function getServiceManifest<Mapped>(
  effects: Effects,
  packageId: PackageId,
  map: (manifest: Manifest | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceManifest<Mapped>
export function getServiceManifest<Mapped>(
  effects: Effects,
  packageId: PackageId,
  map?: (manifest: Manifest | null) => Mapped,
  eq?: (a: Mapped, b: Mapped) => boolean,
): GetServiceManifest<Mapped> {
  return new GetServiceManifest(
    effects,
    packageId,
    map ?? ((a) => a as Mapped),
    eq ?? ((a, b) => deepEqual(a, b)),
  )
}
