import { Injectable } from '@angular/core'
import { Router } from '@angular/router'
import {
  LoadingController,
  ToastController,
  ToastOptions,
} from '@ionic/angular'
import { EMPTY, merge, Observable, ObservableInput } from 'rxjs'
import { filter, pairwise, switchMap, tap } from 'rxjs/operators'
import { ErrorToastService } from '@start9labs/shared'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDataService } from './patch-data.service'
import { DataModel, ServerInfo } from 'src/app/services/patch-db/data-model'

// Watch unread notification count to display toast
@Injectable()
export class UnreadToastService extends Observable<unknown> {
  private unreadToast: HTMLIonToastElement

  private readonly stream$ = this.patchData.pipe(
    switchMap<DataModel | null, ObservableInput<number>>(data => {
      if (data) {
        return this.patch.watch$('server-info', 'unread-notification-count')
      }

      this.unreadToast?.dismiss()

      return EMPTY
    }),
    pairwise(),
    filter(([prev, cur]) => cur > prev),
    tap(() => {
      this.showToast()
    }),
  )

  TOAST: ToastOptions = {
    header: 'Embassy',
    message: `New notifications`,
    position: 'bottom',
    duration: 4000,
    buttons: [
      {
        side: 'start',
        icon: 'close',
        handler: () => true,
      },
      {
        side: 'end',
        text: 'View',
        handler: () => {
          this.router.navigate(['/notifications'], {
            queryParams: { toast: true },
          })
        },
      },
    ],
  }

  constructor(
    private readonly router: Router,
    private readonly patchData: PatchDataService,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
    private readonly embassyApi: ApiService,
    private readonly toastCtrl: ToastController,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private async showToast() {
    await this.unreadToast?.dismiss()

    this.unreadToast = await this.toastCtrl.create(this.TOAST)
    this.unreadToast.buttons?.push()

    await this.unreadToast.present()
  }
}
