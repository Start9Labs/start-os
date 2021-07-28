import { Component } from '@angular/core'
import { AlertController, LoadingController } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { Session } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'sessions',
  templateUrl: 'sessions.page.html',
  styleUrls: ['sessions.page.scss'],
})
export class SessionsPage {
  loading = true
  sessions: { [id: string]: Session } = { }

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
    public readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    // await this.embassyApi.getSessions()
    this.loading = false
  }

  async presentAlertKill (id: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: `Are you sure you want to kill this session?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Kill',
          handler: () => {
            this.kill(id)
          },
        },
      ],
    })
    await alert.present()
  }

  async kill (id: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Killing session...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.killSessions({ ids: [id] })
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
