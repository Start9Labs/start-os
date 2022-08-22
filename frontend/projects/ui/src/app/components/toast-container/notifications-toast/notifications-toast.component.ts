import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { Observable, Subject, merge } from 'rxjs'

import { NotificationsToastService } from './notifications-toast.service'

@Component({
  selector: 'notifications-toast',
  templateUrl: './notifications-toast.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class NotificationsToastComponent {
  private readonly dismiss$ = new Subject<boolean>()

  readonly visible$: Observable<boolean> = merge(
    this.dismiss$,
    this.notifications$,
  )

  constructor(
    @Inject(NotificationsToastService)
    private readonly notifications$: Observable<boolean>,
  ) {}

  onDismiss() {
    this.dismiss$.next(false)
  }
}
