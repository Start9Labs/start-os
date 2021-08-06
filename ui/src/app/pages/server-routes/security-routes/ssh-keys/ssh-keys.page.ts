import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { AlertController, LoadingController } from '@ionic/angular'
import { SSHService } from './ssh.service'
import { Subscription } from 'rxjs'
import { SSHKeys } from 'src/app/services/api/api.types'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'ssh-keys',
  templateUrl: 'ssh-keys.page.html',
  styleUrls: ['ssh-keys.page.scss'],
})
export class SSHKeysPage {
  loading = true
  sshKeys: SSHKeys
  subs: Subscription[] = []
  readonly docsUrl = 'https://docs.start9.com/user-manual/general/developer-options/ssh-setup.html'

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly sshService: SSHService,
    public readonly serverConfig: ServerConfigService,
  ) { }

  async ngOnInit () {
    this.subs = [
      this.sshService.watch$()
      .subscribe(keys => {
        this.sshKeys = keys
      }),
    ]

    await this.sshService.getKeys()

    this.loading = false
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
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
          handler: () => {
            this.delete(hash)
          },
        },
      ],
    })
    await alert.present()
  }

  async delete (hash: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.sshService.delete(hash)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
