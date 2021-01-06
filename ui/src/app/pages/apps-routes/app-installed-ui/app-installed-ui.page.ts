import { Component } from '@angular/core'
import { AppInstalledFull } from 'src/app/models/app-types'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { BehaviorSubject, from, Observable, of, timer } from 'rxjs'
import { catchError, concatMap, delay, distinctUntilChanged, filter, map, take, takeUntil, tap } from 'rxjs/operators'
import { ActivatedRoute } from '@angular/router'
import { ModelPreload } from 'src/app/models/model-preload'
import { NavController, PopoverController } from '@ionic/angular'
import { ServiceUiMenuComponent } from 'src/app/components/service-ui-menu/service-ui-menu.component'
import { AppMetrics } from 'src/app/util/metrics.util'
import { ApiService } from 'src/app/services/api/api.service'
import { AppStatus } from 'src/app/models/app-model'
import { ExtensionBase } from 'src/app/services/extensions/base.extension'
import { Cleanup } from 'src/app/services/extensions/cleanup.extension'
import { DomSanitizer, SafeResourceUrl } from '@angular/platform-browser'
import { ConfigService } from 'src/app/services/config.service'
import { traceWheel } from 'src/app/util/misc.util'


export enum UiPageState {
  // 0 and 1 unfixable (on this page) error states.
  APP_NOT_RUNNING,
  IFRAME_TIMEOUT,

  // 2 + expected states
  LOADING_APP,
  LOADING_IFRAME,
  IFRAME_LOADED,
}
@Component({
  selector: 'app-installed-ui',
  templateUrl: './app-installed-ui.page.html',
  styleUrls: ['./app-installed-ui.page.scss'],
})
export class AppInstalledUiPage extends Cleanup(ExtensionBase) {
  static IFRAME_TIMEOUT = 45000
  appId: string
  AppStatus = AppStatus
  $app$: PropertySubject<AppInstalledFull> = {  } as any
  error: string = undefined

  UiPageState = UiPageState
  $state$ = new BehaviorSubject(UiPageState.LOADING_APP)
  state$ = this.$state$.pipe(
    distinctUntilChanged(),
    delay(0) // fixes an angular change detection quirk in webkit browsers. I don't understand it.
  )

  $properties$: BehaviorSubject<AppMetrics> = new BehaviorSubject({ })
  uiAddress$: Observable<SafeResourceUrl>

  constructor (
    private readonly preload: ModelPreload,
    private readonly popover: PopoverController,
    private readonly apiService: ApiService,
    private readonly config: ConfigService,
    private readonly navCtrl: NavController,
    private readonly sani: DomSanitizer,
    private readonly route: ActivatedRoute,
  ) {  super() }

  updateState(s: UiPageState) {
    const current = this.$state$.getValue()
    if(current <= 1) return
    this.$state$.next(s)
  }

  ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string
    this.cleanup(
      this.preload.appFull(this.appId).pipe(
        tap(app => {
          this.$app$ = app
          this.uiAddress$ = this.$app$.torAddress.pipe(
            traceWheel("address"),
            map(addr => this.sani.bypassSecurityTrustResourceUrl(this.config.isConsulate ? `ext+onion://${addr}` : `http://${addr}`)),
          )
        }),
        concatMap(() => this.getCopyable()),
        map(() => this.loadFrame()),
        concatMap(() => this.$app$.status.pipe(
          filter(s => s !== AppStatus.RUNNING),
          tap(() => this.updateState(UiPageState.APP_NOT_RUNNING))
        ))
      ).subscribe()
    )
  }

  loadFrame(){
    this.updateState(UiPageState.LOADING_IFRAME)

    timer(AppInstalledUiPage.IFRAME_TIMEOUT).pipe(
      takeUntil(this.$state$.pipe(filter(s => s !== UiPageState.LOADING_IFRAME))),
    ).subscribe(() => this.updateState(UiPageState.IFRAME_TIMEOUT))
  }

  frameLoaded (e: any) {
    // This fixes a quirk in webkit browsers where (load) is triggered twice, once immediately prior to actual loading.
    // Luckily, this errant (load) has src === ''
    if(e.target.src === '') return
    this.updateState(UiPageState.IFRAME_LOADED)
  }

  pop: HTMLIonPopoverElement
  async presentPopoverMenu (ev: Event) {
    if (this.pop) await this.pop.dismiss()
    this.pop = await this.popover.create({
      component: ServiceUiMenuComponent,
      componentProps: {
        appId: this.appId,
        properties$: this.$properties$,
        quit: () => this.toServiceShow(),
      },
      showBackdrop: true,
      backdropDismiss: true,
      event: ev,
      cssClass: 'ui-menu',
    })

    await this.pop.present()
  }

  toServiceShow () {
    return this.navCtrl.navigateBack(`/services/installed/${this.appId}`)
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
