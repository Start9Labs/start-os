import { Injectable } from '@angular/core'
import { AlertController, AlertOptions } from '@ionic/angular'
import { EMPTY, from, Observable } from 'rxjs'
import { filter, switchMap } from 'rxjs/operators'
import { Emver } from '@start9labs/shared'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDataService } from './patch-data.service'

// Watch version to refresh browser window
@Injectable()
export class RefreshToastService extends Observable<unknown> {
  private readonly stream$ = this.patchData.pipe(
    switchMap(data =>
      data ? this.patch.watch$('server-info', 'version') : EMPTY,
    ),
    filter(version => !!this.emver.compare(this.config.version, version)),
    switchMap(() => this.getAlert()),
    switchMap(alert => alert.present()),
  )

  constructor(
    private readonly patchData: PatchDataService,
    private readonly patch: PatchDbService,
    private readonly emver: Emver,
    private readonly config: ConfigService,
    private readonly alertCtrl: AlertController,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private getAlert(): Observable<HTMLIonAlertElement> {
    return from(this.alertCtrl.create(ALERT))
  }
}

const ALERT: AlertOptions = {
  backdropDismiss: true,
  header: 'Refresh Needed',
  message:
    'Your user interface is cached and out of date. Hard refresh the page to get the latest UI.',
  buttons: [
    {
      text: 'Refresh Page',
      cssClass: 'enter-click',
      handler: () => {
        location.reload()
      },
    },
  ],
}
