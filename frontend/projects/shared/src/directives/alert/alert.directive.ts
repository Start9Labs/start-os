import { Directive } from '@angular/core'
import {
  AbstractTuiDialogDirective,
  AbstractTuiDialogService,
} from '@taiga-ui/cdk'
import { TuiAlertOptions, TuiAlertService } from '@taiga-ui/core'

// TODO: Move to Taiga UI
@Directive({
  selector: 'ng-template[tuiAlert]',
  providers: [
    {
      provide: AbstractTuiDialogService,
      useExisting: TuiAlertService,
    },
  ],
  inputs: ['options: tuiAlertOptions', 'open: tuiAlert'],
  outputs: ['openChange: tuiAlertChange'],
})
export class TuiAlertDirective<T> extends AbstractTuiDialogDirective<
  TuiAlertOptions<T>
> {}
