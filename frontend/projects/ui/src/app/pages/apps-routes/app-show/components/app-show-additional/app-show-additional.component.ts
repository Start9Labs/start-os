import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, MarkdownComponent } from '@start9labs/shared'
import { from } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-additional',
  templateUrl: 'app-show-additional.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowAdditionalComponent {
  @Input()
  pkg!: PackageDataEntry

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly toastCtrl: ToastController,
    private readonly api: ApiService,
  ) {}

  async copy(address: string): Promise<void> {
    const success = await copyToClipboard(address)
    const message = success
      ? 'Copied to clipboard!'
      : 'Failed to copy to clipboard.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async presentModalLicense() {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'License',
        content: from(this.api.getStatic(this.pkg['static-files']['license'])),
      },
      component: MarkdownComponent,
    })

    await modal.present()
  }
}
