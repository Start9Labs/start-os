import { Component, ElementRef, ViewChild } from '@angular/core'
import { AppInstalledFull } from 'src/app/models/app-types'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { BehaviorSubject, combineLatest, EMPTY, from, Observable, of } from 'rxjs'
import { catchError, concatMap, delay, map, switchMap, tap } from 'rxjs/operators'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { ActivatedRoute } from '@angular/router'
import { ModelPreload } from 'src/app/models/model-preload'
import { NavController, PopoverController } from '@ionic/angular'
import { ServiceUiMenuComponent } from 'src/app/components/service-ui-menu/service-ui-menu.component'
import { AppMetrics } from 'src/app/util/metrics.util'
import { ApiService } from 'src/app/services/api/api.service'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { Cleanup } from 'src/app/util/cleanup'
import { UiComms } from 'src/app/services/ui-comms.service'

@Component({
  selector: 'app-installed-ui',
  templateUrl: './app-installed-ui.page.html',
  styleUrls: ['./app-installed-ui.page.scss'],
})
export class AppInstalledUiPage extends Cleanup {
  AppStatus = AppStatus
  private appId: string
  $app$: PropertySubject<AppInstalledFull> = {  } as any
  error: string = undefined

  $properties$: BehaviorSubject<AppMetrics> = new BehaviorSubject({ })
  $appLoading$ = new BehaviorSubject(true)
  $iframeLoading$ = new BehaviorSubject(true)
  status$: Observable<AppStatus>
  isRunning$: Observable<boolean>

  constructor (
    private readonly route: ActivatedRoute,
    private readonly preload: ModelPreload,
    private readonly popover: PopoverController,
    private readonly apiService: ApiService,
    private readonly appModel: AppModel,
    private readonly uiComms: UiComms,
    private readonly navCtrl: NavController,
  ) {  super() }

  ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string
    this.status$ = this.appModel.watch(this.appId).status.asObservable()
    this.isRunning$ = this.status$.pipe(map(s => s === AppStatus.RUNNING))

    markAsLoadingDuring$(this.$appLoading$,
      this.preload.appFull(this.appId)
      .pipe(
        tap(app => this.$app$ = app),
        tap(() => this.uiComms.$isViewingUi$.next(true)),
        concatMap(() => this.getMetrics()),
      ),
    ).subscribe(
      () => {
        this.cleanup(
          this.appModel.watchForTurnedOn(this.appId).pipe(
            () => this.getMetrics(),
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
    setTimeout(() => this.$iframeLoading$.next(false), 300)
  }

  quit () {
    this.navCtrl.navigateRoot('/services/installed')
  }

  toServiceShow() {
    this.navCtrl.navigateRoot('/services/installed/' + this.appId)
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

    this.uiComms.$isViewingUi$.next(false)
  }

  getMetrics (): Observable<void> {
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
