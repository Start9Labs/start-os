import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { Observable, Subject, merge } from 'rxjs'

import { RefreshAlertService } from './refresh-alert.service'

@Component({
  selector: 'refresh-alert',
  templateUrl: './refresh-alert.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class RefreshAlertComponent {
  private readonly dismiss$ = new Subject<boolean>()

  readonly show$ = merge(this.dismiss$, this.refresh$)

  constructor(
    @Inject(RefreshAlertService) private readonly refresh$: Observable<boolean>,
  ) {}

  onDismiss() {
    this.dismiss$.next(false)
  }
}
