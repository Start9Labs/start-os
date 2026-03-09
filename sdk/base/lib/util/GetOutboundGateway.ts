import { Effects } from '../Effects'
import { Watchable } from './Watchable'

export class GetOutboundGateway extends Watchable<string> {
  protected readonly label = 'GetOutboundGateway'

  constructor(effects: Effects) {
    super(effects)
  }

  protected call(callback?: () => void) {
    return this.effects.getOutboundGateway({ callback })
  }
}
