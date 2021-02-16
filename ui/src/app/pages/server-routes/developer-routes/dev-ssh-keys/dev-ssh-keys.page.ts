import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { AlertController } from '@ionic/angular'
import { LoaderService } from 'src/app/services/loader.service'
import { SSHService } from './ssh.service'

@Component({
  selector: 'dev-ssh-keys',
  templateUrl: 'dev-ssh-keys.page.html',
  styleUrls: ['dev-ssh-keys.page.scss'],
})
export class DevSSHKeysPage {
  error = ''
  loading = true

  constructor (
    private readonly loader: LoaderService,
    private readonly serverConfigService: ServerConfigService,
    private readonly alertCtrl: AlertController,
    public readonly sshService: SSHService,
  ) { }

  ngOnInit () {
    this.sshService.getKeys().then(() => {
      this.loading = false
    })
  }

  async presentModalAdd () {
    await this.serverConfigService.presentModalValueEdit('ssh')
  }

  async presentAlertDelete (hash: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: `Are you sure you want to delete this key?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          cssClass: 'alert-danger',
          handler: () => {
            this.delete(hash)
          },
        },
      ],
    })
    await alert.present()
  }

  async delete (hash: string): Promise<void> {
    this.error = ''
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      await this.sshService.delete(hash)
    }).catch(e => {
      console.error(e)
      this.error = ''
    })
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
