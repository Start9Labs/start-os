import { Component, ViewChild } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { IonContent, ModalController } from '@ionic/angular'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServerConfigService } from 'src/app/services/server-config.service'

@Component({
  selector: 'preferences',
  templateUrl: './preferences.page.html',
  styleUrls: ['./preferences.page.scss'],
})
export class PreferencesPage {
  @ViewChild(IonContent) content: IonContent
  fields = fields
  defaultName: string

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    public readonly serverConfig: ServerConfigService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.defaultName = `Embassy-${this.patch.data['server-info'].id}`
  }

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
        placeholder: this.defaultName,
        nullable: true,
        value: this.patch.data.ui.name,
        buttonText: 'Save',
        loadingText: 'Saving',
        submitFn: (value: string) => this.setDbValue('name', value || this.defaultName),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async setDbValue (key: string, value: string): Promise<void> {
    await this.api.setDbValue({ pointer: `/${key}`, value })
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
