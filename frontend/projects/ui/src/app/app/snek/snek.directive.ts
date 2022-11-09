import { Directive, HostListener, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { SnakePage } from '../../modals/snake/snake.page'
import { ApiService } from '../../services/api/embassy-api.service'

@Directive({
  selector: 'img[appSnek]',
})
export class SnekDirective {
  @Input()
  appSnekHighScore: number | null = null

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) {}

  @HostListener('click')
  async onClick() {
    const modal = await this.modalCtrl.create({
      component: SnakePage,
      cssClass: 'snake-modal',
      backdropDismiss: false,
      componentProps: { highScore: this.appSnekHighScore || 0 },
    })

    modal.onDidDismiss().then(async ({ data }) => {
      if (data?.highScore <= (this.appSnekHighScore || 0)) return

      const loader = await this.loadingCtrl.create({
        message: 'Saving high score...',
        backdropDismiss: true,
      })

      await loader.present()

      try {
        await this.embassyApi.setDbValue<number>(
          ['gaming', 'snake', 'high-score'],
          data.highScore,
        )
      } catch (e: any) {
        this.errToast.present(e)
      } finally {
        this.loadingCtrl.dismiss()
      }
    })

    modal.present()
  }
}
