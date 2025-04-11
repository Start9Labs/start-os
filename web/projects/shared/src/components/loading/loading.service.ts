import { TuiPopoverService } from '@taiga-ui/cdk'
import { Injectable } from '@angular/core'
import { TUI_DIALOGS } from '@taiga-ui/core'

import { LoadingComponent } from './loading.component'

@Injectable({
  providedIn: `root`,
  useFactory: () => new LoadingService(TUI_DIALOGS, LoadingComponent),
})
export class LoadingService extends TuiPopoverService<unknown> {}
