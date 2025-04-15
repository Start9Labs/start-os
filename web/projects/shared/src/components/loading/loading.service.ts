import { TuiPopoverService } from '@taiga-ui/cdk'
import { Injectable } from '@angular/core'
import { TUI_DIALOGS } from '@taiga-ui/core'
import { LoadingComponent } from './loading.component'
import { i18nKey } from '@start9labs/shared'

@Injectable({
  providedIn: `root`,
  useFactory: () => new LoadingService(TUI_DIALOGS, LoadingComponent),
})
export class LoadingService extends TuiPopoverService<unknown> {
  override open<G = void>(textContent: i18nKey) {
    return super.open<G>(textContent)
  }
}
