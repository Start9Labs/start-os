import { Component } from '@angular/core'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppInstalledPreview } from 'src/app/models/app-types'
import { ModelPreload } from 'src/app/models/model-preload'
import { doForAtLeast } from 'src/app/util/misc.util'
import { PropertySubject, PropertySubjectId, toObservable } from 'src/app/util/property-subject.util'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { BehaviorSubject, Observable, Subscription } from 'rxjs'
import { S9Server, ServerModel, ServerStatus } from 'src/app/models/server-model'
import { SyncDaemon } from 'src/app/services/sync.service'
import { Cleanup } from 'src/app/util/cleanup'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'app-installed-list',
  templateUrl: './app-installed-list.page.html',
  styleUrls: ['./app-installed-list.page.scss'],
})
export class AppInstalledListPage extends Cleanup {
  error = ''
  initError = ''
  $loading$ = new BehaviorSubject(true)
  s9Host$: Observable<string>

  AppStatus = AppStatus

  server: PropertySubject<S9Server>
  currentServer: S9Server
  apps: PropertySubjectId<AppInstalledPreview>[] = []

  subsToTearDown: Subscription[] = []

  updatingFreeze = false
  updating = false
  segmentValue: 'services' | 'embassy' = 'services'

  showCertDownload : boolean

  constructor (
    private readonly serverModel: ServerModel,
    private readonly appModel: AppModel,
    private readonly preload: ModelPreload,
    private readonly syncDaemon: SyncDaemon,
    private readonly config: ConfigService,
    private readonly pm: PatchDbModel,
  ) {
    super()
  }

  ngOnDestroy () {
    this.subsToTearDown.forEach(s => s.unsubscribe())
  }

  async ngOnInit () {
    this.server = this.serverModel.watch()
    this.apps = []
    this.cleanup(
      // serverUpdateSubscription
      this.server.status.subscribe(status => {
        if (status === ServerStatus.UPDATING) {
          this.updating = true
        } else {
          if (!this.updatingFreeze) { this.updating = false }
        }
      }),

      // newAppsSubscription
      this.appModel.watchDelta('add').subscribe(({ id }) => {
          if (this.apps.find(a => a.id === id)) return
          this.apps.push({ id, subject: this.appModel.watch(id) })
        },
      ),

      // appsDeletedSubscription
      this.appModel.watchDelta('delete').subscribe(({ id }) => {
        const i = this.apps.findIndex(a => a.id === id)
        this.apps.splice(i, 1)
      }),

      // currentServerSubscription
      toObservable(this.server).subscribe(currentServerProperties => {
        this.currentServer = currentServerProperties
      }),
    )

    markAsLoadingDuring$(this.$loading$, this.preload.apps()).subscribe({
      next: apps => {
        this.apps = apps
      },
      error: e => {
        console.error(e)
        this.error = e.message
      },
    })
  }

  async launchUiTab (id: string, event: Event) {
    event.preventDefault()
    event.stopPropagation()

    const app = this.apps.find(app => app.id === id).subject

    let uiAddress: string
    if (this.config.isTor()) {
      uiAddress = `http://${app.torAddress.getValue()}`
    } else {
      uiAddress = `https://${app.lanAddress.getValue()}`
    }
    return window.open(uiAddress, '_blank')
  }

  async doRefresh (event: any) {
    await doForAtLeast([this.getServerAndApps()], 600)
    event.target.complete()
  }

  async getServerAndApps (): Promise<void> {
    try {
      await this.syncDaemon.sync()
      this.error = ''
    } catch (e) {
      console.error(e)
      this.error = e.message
    }
  }
}
