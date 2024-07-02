import { Directive, HostListener, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
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
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
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

      const loader = this.loader.open('Saving high score...').subscribe()

      try {
        await this.embassyApi.setDbValue<number>(
          ['gaming', 'snake', 'highScore'],
          data.highScore,
        )
      } catch (e: any) {
        this.errorService.handleError(e)
      } finally {
        this.loader.subscribe()
      }
    })

    modal.present()
  }
}
