import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { BehaviorSubject } from 'rxjs'
import { SSHKey } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/apps/ui/modals/generic-input/generic-input.component'

@Component({
  selector: 'ssh-keys',
  templateUrl: 'ssh-keys.page.html',
  styleUrls: ['ssh-keys.page.scss'],
})
export class SSHKeysPage {
  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/ssh'
  sshKeys: SSHKey[] = []
  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
  ) {}

  async ngOnInit() {
    await this.getKeys()
  }

  async getKeys(): Promise<void> {
    try {
      this.sshKeys = await this.embassyApi.getSshKeys({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loading$.next(false)
    }
  }

  async presentModalAdd() {
    const options: GenericInputOptions = {
      title: 'SSH Key',
      message:
        'Enter the SSH public key you would like to authorize for root access to your Embassy.',
      label: '',
      submitFn: (pk: string) => this.add(pk),
    }

    const modal = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      cssClass: 'alertlike-modal',
    })
    await modal.present()
  }

  async add(pubkey: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Saving...',
    })
    await loader.present()

    try {
      const key = await this.embassyApi.addSshKey({ key: pubkey })
      this.sshKeys.push(key)
    } finally {
      loader.dismiss()
    }
  }

  async presentAlertDelete(key: SSHKey, i: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Delete key? This action cannot be undone.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(key, i)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async delete(key: SSHKey, i: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteSshKey({ fingerprint: key.fingerprint })
      this.sshKeys.splice(i, 1)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
