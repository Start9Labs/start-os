import { Effects } from '../Effects'
import * as T from '../types'
import { Watchable } from './Watchable'

export class GetSystemSmtp extends Watchable<T.SmtpValue | null> {
  protected readonly label = 'GetSystemSmtp'

  constructor(effects: Effects) {
    super(effects)
  }

  protected call(callback?: () => void) {
    return this.effects.getSystemSmtp({ callback })
  }
}
