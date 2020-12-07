import { Component } from '@angular/core'
import { NavController, AlertController, ModalController, PopoverController } from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppInstalledFull } from 'src/app/models/app-types'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor, isEmptyObject, modulateTime } from 'src/app/util/misc.util'
import { PropertySubject, peekProperties } from 'src/app/util/property-subject.util'
import { LoaderService, markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { ModelPreload } from 'src/app/models/model-preload'
import { BehaviorSubject, forkJoin, from, fromEvent, of } from 'rxjs'
import { catchError, concatMap, map, take, tap } from 'rxjs/operators'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { Cleanup } from 'src/app/util/cleanup'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { ConfigSpec } from 'src/app/app-config/config-types'
import { ConfigCursor } from 'src/app/app-config/config-cursor'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage extends Cleanup {
  error: { text: string, moreInfo?:
    { title: string, description: string, buttonText: string }
  }

  invalid: string
  $loading$ = new BehaviorSubject(true)
  $loadingText$ = new BehaviorSubject(undefined)

  app: PropertySubject<AppInstalledFull> = { } as any
  appId: string
  hasConfig = false

  recommendation: Recommendation | null = null
  showRecommendation = true
  openRecommendation = false

  edited: boolean
  added: boolean
  rootCursor: ConfigCursor<'object'>
  spec: ConfigSpec
  config: object

  AppStatus = AppStatus

  constructor (
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly wizardBaker: WizardBaker,
    private readonly preload: ModelPreload,
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly alertCtrl: AlertController,
    private readonly modalController: ModalController,
    private readonly trackingModalCtrl: TrackingModalController,
    private readonly popoverController: PopoverController,
    private readonly appModel: AppModel,
  ) { super() }

  backButtonDefense = false

  async ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string

    this.route.params.pipe(take(1)).subscribe(params => {
      if (params.edit) {
        window.history.back()
      }
    })

    this.cleanup(
      fromEvent(window, 'popstate').subscribe(() => {
        this.backButtonDefense = false
        this.trackingModalCtrl.dismissAll()
      }),
      this.trackingModalCtrl.onCreateAny$().subscribe(() => {
        if (!this.backButtonDefense) {
          window.history.pushState(null, null, window.location.href + '/edit')
          this.backButtonDefense = true
        }
      }),
      this.trackingModalCtrl.onDismissAny$().subscribe(() => {
        if (!this.trackingModalCtrl.anyModals && this.backButtonDefense === true) {
          this.navCtrl.back()
        }
      }),
    )

    markAsLoadingDuring$(this.$loading$,
      from(this.preload.appFull(this.appId))
        .pipe(
          tap(app => this.app = app),
          tap(() => this.$loadingText$.next(`Fetching config spec...`)),
          concatMap(() => forkJoin([this.apiService.getAppConfig(this.appId), pauseFor(600)])),
          concatMap(([{ spec, config }]) => {
            const rec = history.state && history.state.configRecommendation as Recommendation
            if (rec) {
              this.$loadingText$.next(`Setting properties to accomodate ${rec.title}...`)
              return from(this.apiService.postConfigureDependency(this.appId, rec.appId, true))
              .pipe(
                map(res => ({
                  spec,
                  config,
                  dependencyConfig: res.config,
                })),
                tap(() => this.recommendation = rec),
                catchError(e => {
                  this.error = { text: `Could not set properties to accomodate ${rec.title}: ${e.message}`, moreInfo: {
                    title: `${rec.title} requires the following:`,
                    description: rec.description,
                    buttonText: 'Configure Manually',
                  } }
                  return of({ spec, config, dependencyConfig: null })
                }),
              )
            } else {
              return of({ spec, config, dependencyConfig: null })
            }
          }),
          map(({ spec, config, dependencyConfig }) => this.setConfig(spec, config, dependencyConfig)),
          tap(() => this.$loadingText$.next(undefined)),
        ),
    ).subscribe({
        error: e => {
          console.error(e)
          this.error = { text: e.message }
        },
      },
    )
  }

  async presentPopover (title: string, description: string, ev: any) {
    const information = `
      <div style="font-size: medium; font-style: italic; margin: 5px 0px;">
        ${title}
      </div>
      <div>
        ${description}
      </div>
    `
    const popover = await this.popoverController.create({
      component: InformationPopoverComponent,
      event: ev,
      translucent: false,
      showBackdrop: true,
      backdropDismiss: true,
      componentProps: {
        information,
      },
    })
    return await popover.present()
  }

  setConfig (spec: ConfigSpec, config: object, dependencyConfig?: object) {
    this.rootCursor = dependencyConfig ? new ConfigCursor(spec, config, null, dependencyConfig) : new ConfigCursor(spec, config)
    this.spec = this.rootCursor.spec().spec
    this.config = this.rootCursor.config()
    this.handleObjectEdit()
    this.hasConfig = !isEmptyObject(this.spec)
  }

  dismissRecommendation () {
    this.showRecommendation = false
  }

  dismissError () {
    this.error = undefined
  }

  async cancel () {
    if (this.edited) {
      await this.presentAlertUnsaved()
    } else {
      this.navCtrl.back()
    }
  }

  async save () {
    const app = peekProperties(this.app)
    const ogAppStatus = app.status

    return this.loader.of({
      message: `Saving config...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      const config = this.config
      const { breakages } = await this.apiService.patchAppConfig(app, config, true)

      if (breakages.length) {
        const { cancelled } = await wizardModal(
          this.modalController,
          this.wizardBaker.configure({
            app,
            breakages,
          }),
        )
        if (cancelled) return { skip: true }
      }

      return this.apiService.patchAppConfig(app, config).then(
        () => this.preload.loadInstalledApp(this.appId).then(() => ({ skip: false })),
      )
    })
    .then(({ skip }) => {
      if (skip) return
      if (ogAppStatus === AppStatus.RUNNING) {
        this.appModel.update({ id: this.appId, status: AppStatus.RESTARTING }, modulateTime(new Date(), 3, 'seconds'))
      }
      this.navCtrl.back()
    })
    .catch(e => this.error = { text: e.message })
  }

  handleObjectEdit () {
    this.edited = this.rootCursor.isEdited()
    this.added = this.rootCursor.isNew()
    this.invalid = this.rootCursor.checkInvalid()
  }

  private async presentAlertUnsaved () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Unsaved Changes',
      message: 'You have unsaved changes. Are you sure you want to leave?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: `Leave`,
          cssClass: 'alert-danger',
          handler: () => {
            this.navCtrl.back()
          },
        },
      ],
    })
    await alert.present()
  }
}

