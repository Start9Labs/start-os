import { inject, Injectable, signal } from '@angular/core'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'

@Injectable({
  providedIn: 'root',
})
export class SidebarService {
  readonly start = signal(false)
  readonly end = signal(!inject(TUI_IS_MOBILE))
}
