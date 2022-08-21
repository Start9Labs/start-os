import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import {
  LoadingController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/modals/generic-input/generic-input.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { LocalStorageService } from '../../../services/local-storage.service'

@Component({
  selector: 'preferences',
  templateUrl: './preferences.page.html',
  styleUrls: ['./preferences.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PreferencesPage {
  clicks = 0

  readonly ui$ = this.patch.watch$('ui')
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly localStorageService: LocalStorageService,
    private readonly patch: PatchDbService,
    readonly serverConfig: ServerConfigService,
  ) {}

  async presentModalName(
    placeholder: string,
    initialValue: string,
  ): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Edit Device Name',
      message: 'This is for your reference only.',
      label: 'Device Name',
      useMask: false,
      placeholder,
      nullable: true,
      initialValue,
      buttonText: 'Save',
      submitFn: (value: string) =>
        this.setDbValue('name', value || placeholder),
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async setDbValue(key: string, value: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Saving...',
    })
    await loader.present()

    try {
      await this.api.setDbValue({ pointer: `/${key}`, value })
    } finally {
      loader.dismiss()
    }
  }

  async addClick() {
    this.clicks++
    if (this.clicks >= 5) {
      this.clicks = 0
      const newVal = await this.localStorageService.toggleShowDevTools()
      const toast = await this.toastCtrl.create({
        header: newVal ? 'Dev tools unlocked' : 'Dev tools hidden',
        position: 'bottom',
        duration: 1000,
      })

      await toast.present()
    }
    setTimeout(() => {
      this.clicks = Math.max(this.clicks - 1, 0)
    }, 10000)
  }
}
