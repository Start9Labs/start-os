import { Effects } from '../Effects'
import { Host, HostId, PackageId } from '../osBindings'
import { Watchable } from './Watchable'

export class GetHostInfo extends Watchable<Host | null> {
  protected readonly label = 'GetHostInfo'

  constructor(
    effects: Effects,
    readonly opts: { hostId: HostId; packageId?: PackageId },
  ) {
    super(effects)
  }

  protected fetch(callback?: () => void) {
    return this.effects.getHostInfo({ ...this.opts, callback })
  }
}
