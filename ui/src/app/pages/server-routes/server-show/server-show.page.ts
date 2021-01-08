import { Component } from '@angular/core'
import { LoadingOptions } from '@ionic/core'
import { ServerModel, ServerStatus } from 'src/app/models/server-model'
import { AlertController } from '@ionic/angular'
import { S9Server } from 'src/app/models/server-model'
import { ApiService } from 'src/app/services/api/api.service'
import { SyncDaemon } from 'src/app/services/sync.service'
import { Subscription, Observable } from 'rxjs'
import { PropertySubject, toObservable } from 'src/app/util/property-subject.util'
import { doForAtLeast } from 'src/app/util/misc.util'
import { LoaderService } from 'src/app/services/loader.service'
import { Emver } from 'src/app/services/emver.service'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  error = ''
  s9Host$: Observable<string>

  server: PropertySubject<S9Server>
  currentServer: S9Server

  subsToTearDown: Subscription[] = []

  updatingFreeze = false
  updating = false

  constructor (
    private readonly serverModel: ServerModel,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoaderService,
    private readonly apiService: ApiService,
    private readonly syncDaemon: SyncDaemon,
    private readonly emver: Emver,
  ) { }

  async ngOnInit () {
    this.server = this.serverModel.watch()
    this.subsToTearDown.push(
      // serverUpdateSubscription
      this.server.status.subscribe(status => {
        if (status === ServerStatus.UPDATING) {
          this.updating = true
        } else {
          if (!this.updatingFreeze) { this.updating = false }
        }
      }),
      // currentServerSubscription
      toObservable(this.server).subscribe(currentServerProperties => {
        this.currentServer = currentServerProperties
      }),
    )
  }

  ionViewDidEnter () {
    this.error = ''
  }

  ngOnDestroy () {
    this.subsToTearDown.forEach(s => s.unsubscribe())
  }

  async doRefresh (event: any) {
    await doForAtLeast([this.getServerAndApps()], 600)
    event.target.complete()
  }

  async getServerAndApps (): Promise<void> {
    try {
      this.syncDaemon.sync()
      this.error = ''
    } catch (e) {
      console.error(e)
      this.error = e.message
    }
  }

  async checkForUpdates (): Promise<void> {
    const loader = await this.loader.ctrl.create(LoadingSpinner('Checking for updates...'))
    await loader.present()

    try {
      const { versionLatest } = await this.apiService.getVersionLatest()
      if (this.emver.compare(this.server.versionInstalled.getValue(), versionLatest) === -1) {
        this.presentAlertUpdate(versionLatest)
      } else {
        this.presentAlertUpToDate()
      }
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      await loader.dismiss()
    }
  }

  async presentAlertUpToDate () {
    const alert = await this.alertCtrl.create({
      header: 'Up To Date',
      message: `You are running the latest version of EmbassyOS!`,
      buttons: ['OK'],
    })
    await alert.present()
  }

  async presentAlertUpdate (versionLatest: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Confirm',
      message: `Update EmbassyOS to ${versionLatest}?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Update',
          handler: () => {
            this.updateEmbassyOS(versionLatest)
          },
        },
      ],
    })
    await alert.present()
  }

  async presentAlertRestart () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Confirm',
      message: `Are you sure you want to restart your Embassy?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Restart',
          cssClass: 'alert-danger',
          handler: () => {
            this.restart()
          },
        },
      ]},
    )
    await alert.present()
  }

  async presentAlertShutdown () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Confirm',
      message: `Are you sure you want to shut down your Embassy? To turn it back on, you will need to physically unplug the device and plug it back in.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Shutdown',
          cssClass: 'alert-danger',
          handler: () => {
            this.shutdown()
          },
        },
      ],
    })
    await alert.present()
  }

  private async updateEmbassyOS (versionLatest: string) {
    this.loader
        .displayDuringAsync(async () => {
          await this.apiService.updateAgent(versionLatest)
          this.serverModel.update({ status: ServerStatus.UPDATING })
          // hides the "Update Embassy to..." button for this intance of the component
          this.updatingFreeze = true
          this.updating = true
          setTimeout(() => this.updatingFreeze = false, 8000)
        })
        .catch(e => this.setError(e))
  }

  private async restart () {
    this.loader
        .of(LoadingSpinner(`Restarting ${this.currentServer.name}...`))
        .displayDuringAsync( async () => {
            this.serverModel.markUnreachable()
            await this.apiService.restartServer()
        })
        .catch(e => this.setError(e))
  }

  private async shutdown () {
    this.loader
        .of(LoadingSpinner(`Shutting down ${this.currentServer.name}...`))
        .displayDuringAsync( async () => {
          this.serverModel.markUnreachable()
          await this.apiService.shutdownServer()
        })
        .catch(e => this.setError(e))
  }

  setError (e: Error) {
    console.error(e)
    this.error = e.message
  }
}

const LoadingSpinner: (m?: string) => LoadingOptions = (m) => {
  const toMergeIn = m ? { message: m } : { }
  return {
    spinner: 'lines',
    cssClass: 'loader',
    ...toMergeIn,
  } as LoadingOptions
}


