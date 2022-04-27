import { Injectable } from '@angular/core'
import { Router } from '@angular/router'
import {
  LoadingController,
  ToastController,
  ToastOptions,
} from '@ionic/angular'
import { EMPTY, merge, Observable } from 'rxjs'
import { filter, pairwise, switchMap, tap } from 'rxjs/operators'
import { ErrorToastService } from '@start9labs/shared'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDataService } from './patch-data.service'

// Watch unread notification count to display toast
@Injectable()
export class UnreadToastService extends Observable<unknown> {
  private unreadToast: HTMLIonToastElement

  private readonly stream$ = this.patchData.pipe(
    switchMap(data => {
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

    this.unreadToast = await this.toastCtrl.create(TOAST)
    this.unreadToast.buttons.push({
      side: 'end',
      text: 'View',
      handler: () => {
        this.router.navigate(['/notifications'], {
          queryParams: { toast: true },
        })
      },
    })

    await this.unreadToast.present()
  }
}

const TOAST: ToastOptions = {
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
  ],
}
