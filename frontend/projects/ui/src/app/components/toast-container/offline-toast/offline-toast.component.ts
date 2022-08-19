import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { Observable, Subject, merge } from 'rxjs'

import { OfflineMessage, OfflineToastService } from './offline-toast.service'

@Component({
  selector: 'offline-toast',
  templateUrl: './offline-toast.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OfflineToastComponent {
  private readonly dismiss$ = new Subject<null>()

  readonly message$: Observable<OfflineMessage | null> = merge(
    this.dismiss$,
    this.failure$,
  )

  constructor(
    @Inject(OfflineToastService)
    private readonly failure$: Observable<OfflineMessage | null>,
  ) {}

  onDismiss() {
    this.dismiss$.next(null)
  }
}
