import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { merge, Observable, Subject } from 'rxjs'

import { RefreshAlertService } from './refresh-alert.service'
import { SwUpdate } from '@angular/service-worker'
import { LoadingController } from '@ionic/angular'

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
    private readonly loadingCtrl: LoadingController,
  ) {}

  ngOnInit() {
    this.onPwa = window.matchMedia('(display-mode: standalone)').matches
  }

  async pwaReload() {
    const loader = await this.loadingCtrl.create({
      message: 'Reloading PWA...',
    })
    await loader.present()
    try {
      // attempt to update to the latest client version available
      await this.updates.activateUpdate()
    } catch (e) {
      console.error('Error activating update from service worker: ', e)
    } finally {
      loader.dismiss()
      // always reload, as this resolves most out of sync cases
      window.location.reload()
    }
  }

  onDismiss() {
    this.dismiss$.next(false)
  }
}
