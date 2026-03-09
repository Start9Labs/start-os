import { Effects } from '../Effects'
import { PackageId, StatusInfo } from '../osBindings'
import { Watchable } from './Watchable'

export class GetStatus extends Watchable<StatusInfo | null> {
  protected readonly label = 'GetStatus'

  constructor(
    effects: Effects,
    readonly opts: { packageId?: PackageId } = {},
  ) {
    super(effects)
  }

  protected call(callback?: () => void) {
    return this.effects.getStatus({ ...this.opts, callback })
  }
}
