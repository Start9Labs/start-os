import { Component, Input } from '@angular/core'
import { AppInstalledFull } from 'src/app/models/app-types'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { BehaviorSubject, from, Observable, of } from 'rxjs'
import { catchError, concatMap, map, take, tap } from 'rxjs/operators'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { ActivatedRoute } from '@angular/router'
import { ModelPreload } from 'src/app/models/model-preload'
import { NavController, PopoverController } from '@ionic/angular'
import { ServiceUiMenuComponent } from 'src/app/components/service-ui-menu/service-ui-menu.component'
import { AppMetrics } from 'src/app/util/metrics.util'
import { ApiService } from 'src/app/services/api/api.service'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { ExtensionBase } from 'src/app/services/extensions/base.extension'
import { Cleanup } from 'src/app/services/extensions/cleanup.extension'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { DomSanitizer, SafeResourceUrl } from '@angular/platform-browser'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'app-installed-ui',
  templateUrl: './app-installed-ui.page.html',
  styleUrls: ['./app-installed-ui.page.scss'],
})
export class AppInstalledUiPage extends Cleanup(ExtensionBase) {
  @Input()
  appId: string
  AppStatus = AppStatus
  $app$: PropertySubject<AppInstalledFull> = {  } as any
  error: string = undefined

  $properties$: BehaviorSubject<AppMetrics> = new BehaviorSubject({ })
  $appLoading$ = new BehaviorSubject(false)
  $iframeLoading$ = new BehaviorSubject(true)
  status$: Observable<AppStatus>
  isRunning$: Observable<boolean>

  uiAddress$: Observable<SafeResourceUrl>

  constructor (
    private readonly preload: ModelPreload,
    private readonly popover: PopoverController,
    private readonly apiService: ApiService,
    private readonly appModel: AppModel,
    private readonly config: ConfigService,
    private readonly navCtrl: NavController,
    private readonly trackingModalCtrl: TrackingModalController,
    private readonly route: ActivatedRoute,
    private readonly sani: DomSanitizer,
  ) {  super() }

  ngOnInit () {
    this.status$ = this.appModel.watch(this.appId).status.asObservable()
    this.isRunning$ = this.status$.pipe(map(s => s === AppStatus.RUNNING))

    this.route.params.pipe(take(1)).subscribe(params => {
      if (params.ui) {
        window.history.back()
      }
    })

    markAsLoadingDuring$(this.$appLoading$,
      this.preload.appFull(this.appId)
      .pipe(
        tap(app => {
          this.$app$ = app
          this.uiAddress$ = this.$app$.torAddress.pipe(
            map(addr => this.sani.bypassSecurityTrustResourceUrl(this.config.isConsulate ? `ext+onion://${addr}` : `http://${addr}`))
          )
        }),
        concatMap(() => this.getCopyable()),
      ),
    ).subscribe(
      () => {
        this.cleanup(
          this.appModel.watchForTurnedOn(this.appId).pipe(
            () => this.getCopyable(),
          ).subscribe(),
        )
      })
  }

  pop: HTMLIonPopoverElement
  async presentPopoverMenu (ev: Event) {
    if (this.pop) await this.pop.dismiss()
    this.pop = await this.popover.create({
      component: ServiceUiMenuComponent,
      componentProps: {
        appId: this.appId,
        properties$: this.$properties$,
        quit: () => this.quit(),
      },
      showBackdrop: true,
      backdropDismiss: true,
      event: ev,
      cssClass: 'ui-menu',
    })

    await this.pop.present()
  }

  iframeLoaded () {
    this.$iframeLoading$.next(false)
  }

  quit () {
    return this.trackingModalCtrl.dismiss()
  }

  async toServiceShow () {
    await this.navCtrl.navigateRoot('/services/installed/' + this.appId)
    return this.trackingModalCtrl.dismiss()
  }

  iframe: HTMLIFrameElement | null
  getIframe () {
    if (!this.iframe) {
      this.iframe = document.getElementById(`${this.appId}-ui`) as HTMLIFrameElement
    }
    return this.iframe
  }

  ionViewDidLeave () {
    if (this.pop) this.pop.dismiss()

    const iframe = this.getIframe()
    if (iframe) { iframe.src = 'about:blank' }
  }

  getCopyable (): Observable<void> {
    return from(this.apiService.getAppMetrics(this.appId)).pipe(
      tap(() => this.error = undefined),
      map(metrics => this.$properties$.next(metrics)),
      catchError(e => {
        this.error = e.message
        return of(console.error(`Exception in metrics polling`, e))
      }),
    )
  }
}
