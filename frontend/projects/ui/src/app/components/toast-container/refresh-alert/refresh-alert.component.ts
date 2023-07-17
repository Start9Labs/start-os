import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { merge, Observable, Subject } from 'rxjs'

import { RefreshAlertService } from './refresh-alert.service'
import { SwUpdate } from '@angular/service-worker'

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
    private readonly updates: SwUpdate,
  ) {}

  async pwaReload() {
    try {
      await this.updates.activateUpdate()
    } catch (e) {
      console.error('Error activating update from service worker: ', e)
    } finally {
      window.location.reload()
    }
  }

  onDismiss() {
    this.dismiss$.next(false)
  }
}
