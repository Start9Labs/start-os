import { Component } from '@angular/core'
import { AlertController, LoadingController, ModalController } from '@ionic/angular'
import { SSHKey } from 'src/app/services/api/api.types'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'

@Component({
  selector: 'ssh-keys',
  templateUrl: 'ssh-keys.page.html',
  styleUrls: ['ssh-keys.page.scss'],
})
export class SSHKeysPage {
  loading = true
  sshKeys: SSHKey[] = []
  readonly docsUrl = 'https://docs.start9.com/user-manual/general/developer-options/ssh-setup.html'

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) { }

  async ngOnInit () {
    await this.getKeys()
  }

  async getKeys (): Promise<void> {
    try {
      this.sshKeys = await this.embassyApi.getSshKeys({ })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async presentModalAdd () {
    const { name, description } = sshSpec

    const modal = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: {
        title: name,
        message: description,
        label: name,
        loadingText: 'Saving',
        submitFn: (pk: string) => this.add(pk),
      },
      cssClass: 'alertlike-modal',
    })
    await modal.present()
  }

  async add (pubkey: string): Promise<void> {
    const key = await this.embassyApi.addSshKey({ key: pubkey })
    this.sshKeys.push(key)
  }

  async presentAlertDelete (i: number) {
    const alert = await this.alertCtrl.create({
      header: 'Caution',
      message: `Are you sure you want to delete this key?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(i)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async delete (i: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const entry = this.sshKeys[i]
      await this.embassyApi.deleteSshKey({ fingerprint: entry.fingerprint })
      this.sshKeys.splice(i, 1)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

const sshSpec = {
  type: 'string',
  name: 'SSH Key',
  description: 'Enter the SSH public key of you would like to authorize for root access to your Embassy.',
  nullable: false,
  // @TODO regex for SSH Key
  // pattern: '',
  'pattern-description': 'Must be a valid SSH key',
  masked: false,
  copyable: false,
}
