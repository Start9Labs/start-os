import { Injectable } from '@angular/core'
import {
  LoadingController,
  LoadingOptions,
  ToastController,
  ToastOptions,
} from '@ionic/angular'
import { EMPTY, Observable } from 'rxjs'
import { distinctUntilChanged, filter, switchMap } from 'rxjs/operators'
import { ErrorToastService } from '@start9labs/shared'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDataService } from './patch-data.service'

// Watch status to present toast for updated state
@Injectable()
export class UpdateToastService extends Observable<unknown> {
  private updateToast?: HTMLIonToastElement

  private readonly stream$ = this.patchData.pipe(
    switchMap(data => {
      if (data) {
        return this.patch.watch$('server-info', 'status-info', 'updated')
      }

      this.errToast.dismiss()
      this.updateToast?.dismiss()

      return EMPTY
    }),
    distinctUntilChanged(),
    filter(Boolean),
    switchMap(() => this.showToast()),
  )

  constructor(
    private readonly patchData: PatchDataService,
    private readonly patch: PatchDbService,
    private readonly embassyApi: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  LOADER: LoadingOptions = {
    message: 'Restarting...',
  }

  TOAST: ToastOptions = {
    header: 'EOS download complete!',
    message:
      'Restart your Embassy for these updates to take effect. It can take several minutes to come back online.',
    position: 'bottom',
    duration: 0,
    cssClass: 'success-toast',
    buttons: [
      {
        side: 'start',
        icon: 'close',
        handler: () => true,
      },
      {
        side: 'end',
        text: 'Restart',
        handler: () => {
          this.restart()
        },
      },
    ],
  }
  private async showToast() {
    await this.updateToast?.dismiss()

    this.updateToast = await this.toastCtrl.create(this.TOAST)

    await this.updateToast.present()
  }

  private async restart(): Promise<void> {
    const loader = await this.loadingCtrl.create(this.LOADER)

    await loader.present()

    try {
      await this.embassyApi.restartServer({})
    } catch (e: any) {
      await this.errToast.present(e)
    } finally {
      await loader.dismiss()
    }
  }
}
