import { Effects } from '../Effects'
import { Manifest, PackageId } from '../osBindings'
import { deepEqual } from './deepEqual'
import { Watchable } from './Watchable'

export class GetServiceManifest<
  Mapped = Manifest | null,
> extends Watchable<Manifest | null, Mapped> {
  protected readonly label = 'GetServiceManifest'

  constructor(
    effects: Effects,
    readonly opts: { packageId: PackageId },
    options?: {
      map?: (value: Manifest | null) => Mapped
      eq?: (a: Mapped, b: Mapped) => boolean
    },
  ) {
    super(effects, options)
  }

  protected fetch(callback?: () => void) {
    return this.effects.getServiceManifest({ ...this.opts, callback })
  }
}

export function getServiceManifest(
  effects: Effects,
  packageId: PackageId,
): GetServiceManifest<Manifest | null>
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
  return new GetServiceManifest<Mapped>(effects, { packageId }, {
    map: map ?? ((a) => a as Mapped),
    eq: eq ?? ((a, b) => deepEqual(a, b)),
  })
}
