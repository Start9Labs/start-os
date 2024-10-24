import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { SwUpdate } from '@angular/service-worker'
import { LoadingService } from '@start9labs/shared'
import { merge, Observable, Subject } from 'rxjs'

import { RefreshAlertService } from './refresh-alert.service'

@Component({
  selector: 'refresh-alert',
  templateUrl: './refresh-alert.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class RefreshAlertComponent {
  private readonly dismiss$ = new Subject<boolean>()
  readonly show$ = merge(this.dismiss$, this.refresh$)
  onPwa = false

  constructor(
    @Inject(RefreshAlertService) private readonly refresh$: Observable<boolean>,
    private readonly updates: SwUpdate,
    private readonly loader: LoadingService,
  ) {}

  ngOnInit() {
    this.onPwa = window.matchMedia('(display-mode: standalone)').matches
  }

  async pwaReload() {
    const loader = this.loader.open('Reloading PWA...').subscribe()
    try {
      // attempt to update to the latest client version available
      await this.updates.activateUpdate()
    } catch (e) {
      console.error('Error activating update from service worker: ', e)
    } finally {
      loader.unsubscribe()
      // always reload, as this resolves most out of sync cases
      window.location.reload()
    }
  }

  onDismiss() {
    this.dismiss$.next(false)
  }
}
