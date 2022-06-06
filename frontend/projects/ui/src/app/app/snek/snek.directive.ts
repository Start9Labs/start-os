import { Directive, HostListener } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'

import { SnakePage } from '../../modals/snake/snake.page'
import { PatchDbService } from '../../services/patch-db/patch-db.service'
import { ApiService } from '../../services/api/embassy-api.service'

@Directive({
  selector: 'img[appSnek]',
})
export class SnekDirective {
  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) {}

  @HostListener('click')
  async onClick() {
    const modal = await this.modalCtrl.create({
      component: SnakePage,
      cssClass: 'snake-modal',
      backdropDismiss: false,
    })

    modal.onDidDismiss().then(async ({ data }) => {
      const highScore =
        this.patch.getData().ui.gaming?.snake?.['high-score'] || 0

      if (data?.highScore > highScore) {
        const loader = await this.loadingCtrl.create({
          spinner: 'lines',
          message: 'Saving High Score...',
        })

        await loader.present()

        try {
          await this.embassyApi.setDbValue({
            pointer: '/gaming',
            value: { snake: { 'high-score': data.highScore } },
          })
        } catch (e: any) {
          this.errToast.present(e)
        } finally {
          this.loadingCtrl.dismiss()
        }
      }
    })

    modal.present()
  }
}
