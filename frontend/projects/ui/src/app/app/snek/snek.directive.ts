import { Directive, HostListener } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'

import { SnakePage } from '../../modals/snake/snake.page'
import { PatchDbService } from '../../services/patch-db/patch-db.service'
import { ApiService } from '../../services/api/embassy-api.service'

const SNEK = ['s', 'n', 'e', 'k']

@Directive({
  selector: 'img[appSnek]',
})
export class SnekDirective {
  private readonly code = new Map<string, boolean>([
    ...SNEK.map<[string, boolean]>(char => [char, false]),
    ['unlocked', false],
  ])

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) {}

  @HostListener('document:keyup', ['$event.key'])
  onKeyUp(key: string) {
    this.code.set(key, false)
  }

  @HostListener('document:keypress', ['$event'])
  async onKeyPress({ repeat, key }: KeyboardEvent) {
    if (repeat || this.code.get('unlocked')) return

    this.code.set(key, true)

    if (SNEK.every(char => this.code.get(char))) {
      await this.openSnek()
    }
  }

  @HostListener('click')
  async onClick() {
    await this.openSnek()
  }

  private async openSnek() {
    this.code.set('unlocked', true)

    const modal = await this.modalCtrl.create({
      component: SnakePage,
      cssClass: 'snake-modal',
      backdropDismiss: false,
    })

    modal.onDidDismiss().then(async ({ data }) => {
      this.code.set('unlocked', false)

      const highScore =
        this.patch.getData().ui.gaming?.snake?.['high-score'] || 0

      if (data?.highScore > highScore) {
        const loader = await this.loadingCtrl.create({
          spinner: 'lines',
          cssClass: 'loader',
          message: 'Saving High Score...',
        })

        await loader.present()

        try {
          await this.embassyApi.setDbValue({
            pointer: '/gaming',
            value: { snake: { 'high-score': data.highScore } },
          })
        } catch (e) {
          this.errToast.present(e)
        } finally {
          this.loadingCtrl.dismiss()
        }
      }
    })

    modal.present()
  }
}
