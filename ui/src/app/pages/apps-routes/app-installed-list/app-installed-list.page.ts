import { Component } from '@angular/core'
import { AppModel } from 'src/app/models/app-model'
import { AppInstalledPreview } from 'src/app/models/app-types'
import { ModelPreload } from 'src/app/models/model-preload'
import { doForAtLeast, pauseFor } from 'src/app/util/misc.util'
import { peekProperties, PropertySubject, PropertySubjectId, toObservable } from 'src/app/util/property-subject.util'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { BehaviorSubject, Observable, Subscription } from 'rxjs'
import { S9Server, ServerModel, ServerStatus } from 'src/app/models/server-model'
import { SyncDaemon } from 'src/app/services/sync.service'
import { Cleanup } from 'src/app/util/cleanup'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'app-installed-list',
  templateUrl: './app-installed-list.page.html',
  styleUrls: ['./app-installed-list.page.scss'],
})
export class AppInstalledListPage extends Cleanup {
  swiped = false
  error = ''
  initError = ''
  $loading$ = new BehaviorSubject(true)
  s9Host$: Observable<string>

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
    private readonly navCtrl: NavController,
  ) {
    super()
  }

  ionViewDidEnter () {
    this.swiped = false
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

  goToUi = false
  handlePan (app: PropertySubjectId<AppInstalledPreview>, ev: any) {
    if (!app.subject.ui.getValue()) return
    const card = document.getElementById(app.id)
    const threshold = 0.8 * card.offsetHeight

    if (ev.deltaY >= threshold) this.goToUi = true

    card.style.top = dampenPosition(0.5 * card.offsetHeight, ev.deltaY) + 'px'

    if (ev.isFinal) {
      card.style.transition = '0.5s cubic-bezier(0.26, 0.74, 0.4, 0.88) all'
      if (this.goToUi) {
        card.style.top = '0px'
        card.style.opacity = '0'
        pauseFor(500)
          .then(() => this.viewServiceUI(app.id))
          .then(() => pauseFor(500))
          .then(() => card.style.opacity = '1')
      } else {
        card.style.top = 0 + 'px'
      }

      pauseFor(500).then(() => {
        card.style.transition = 'unset'
      })
    }
  }

  async viewServiceUI (appId: string) {
    this.navCtrl.navigateForward(`/services/installed/${appId}/ui`)
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

export function dampenPosition (boundary: number, delta: number): number {
  return Math.max(0, boundary + 50 * Math.log(delta / 50 + 1))
}
