import { Effects } from '../Effects'
import { Manifest, PackageId } from '../osBindings'
import { Watchable } from './Watchable'

export class GetServiceManifest extends Watchable<Manifest> {
  protected readonly label = 'GetServiceManifest'

  constructor(
    effects: Effects,
    readonly opts: { packageId: PackageId },
  ) {
    super(effects)
  }

  protected call(callback?: () => void) {
    return this.effects.getServiceManifest({ ...this.opts, callback })
  }
}
