import { Component, ViewChild } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { IonContent, LoadingController, ModalController } from '@ionic/angular'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'preferences',
  templateUrl: './preferences.page.html',
  styleUrls: ['./preferences.page.scss'],
})
export class PreferencesPage {
  @ViewChild(IonContent) content: IonContent
  fields = fields

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
    public readonly patch: PatchDbService,
  ) { }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  async presentModalName (): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Edit Device Name',
        message: 'This is for your reference only.',
        label: 'Device Name',
        useMask: false,
        placeholder: this.patch.data['server-info'].id,
        nullable: true,
        value: this.patch.data.ui.name,
        buttonText: 'Save',
        submitFn: async (value: string) => await this.setDbValue('name', value || this.patch.data['server-info'].id),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async setDbValue (key: string, value: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.setDbValue({ pointer: `/${key}`, value })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

const fields: ConfigSpec = {
  'name': {
    name: 'Device Name',
    type: 'string',
    nullable: false,
    masked: false,
    copyable: false,
  },
}
