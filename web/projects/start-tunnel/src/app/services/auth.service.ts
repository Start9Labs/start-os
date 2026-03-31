import { effect, inject, Injectable, signal } from '@angular/core'
import { WA_LOCAL_STORAGE } from '@ng-web-apis/common'

const KEY = '_startos/tunnel-loggedIn'

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private readonly storage = inject(WA_LOCAL_STORAGE)
  private readonly effect = effect(() => {
    if (this.authenticated()) {
      this.storage?.setItem(KEY, JSON.stringify(true))
    } else {
      this.storage?.removeItem(KEY)
    }
  })

  readonly authenticated = signal(Boolean(this.storage?.getItem(KEY)))
}
