import { Component, ViewChild } from '@angular/core'
import { NavController, AlertController, ModalController, IonContent, LoadingController } from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { isEmptyObject, Recommendation } from 'src/app/util/misc.util'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { from, fromEvent, of, Subscription } from 'rxjs'
import { catchError, concatMap, map, take, tap } from 'rxjs/operators'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage {
  error: { text: string, moreInfo?:
    { title: string, description: string, buttonText: string }
  }

  loadingText: string | undefined

  pkg: PackageDataEntry
  hasConfig = false

  mocalShowing = false
  packageState = PackageState

  rec: Recommendation | null = null
  showRec = true
  openRec = false

  invalid: string
  edited: boolean
  added: boolean
  rootCursor: ConfigCursor<'object'>
  spec: ConfigSpec
  config: object

  @ViewChild(IonContent) content: IonContent
  subs: Subscription[] = []

  constructor (
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly wizardBaker: WizardBaker,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly modalController: ModalController,
    private readonly trackingModalCtrl: TrackingModalController,
    private readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId') as string

    this.subs = [
      this.route.params.pipe(take(1)).subscribe(params => {
        if (params.edit) {
          window.history.back()
        }
      }),
      fromEvent(window, 'popstate').subscribe(() => {
        this.mocalShowing = false
        this.trackingModalCtrl.dismissAll()
      }),
      this.trackingModalCtrl.onCreateAny$().subscribe(() => {
        if (!this.mocalShowing) {
          window.history.pushState(null, null, window.location.href + '/edit')
          this.mocalShowing = true
        }
      }),
      this.trackingModalCtrl.onDismissAny$().subscribe(() => {
        if (!this.trackingModalCtrl.anyModals && this.mocalShowing === true) {
          this.navCtrl.back()
        }
      }),
      this.patch.watch$('package-data', pkgId)
      .pipe(
        tap(pkg => this.pkg = pkg),
        tap(() => this.loadingText = 'Loading config...'),
        concatMap(() => this.embassyApi.getPackageConfig({ id: pkgId })),
        concatMap(({ spec, config }) => {
          const rec = history.state && history.state.configRecommendation as Recommendation
          if (rec) {
            this.loadingText = `Setting properties to accommodate ${rec.dependentTitle}...`
            return from(this.embassyApi.dryConfigureDependency({ 'dependency-id': pkgId, 'dependent-id': rec.dependentId }))
            .pipe(
              map(res => ({
                spec,
                config,
                dependencyConfig: res,
              })),
              tap(() => this.rec = rec),
              catchError(e => {
                this.error = { text: `Could not set properties to accommodate ${rec.dependentTitle}: ${e.message}`, moreInfo: {
                  title: `${rec.dependentTitle} requires the following:`,
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
        tap(() => this.loadingText = undefined),
        take(1),
      ).subscribe({
        error: e => {
          console.error(e.message)
          this.error = { text: e.message }
        },
      }),
    ]
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  setConfig (spec: ConfigSpec, config: object, dependencyConfig?: object) {
    this.rootCursor = dependencyConfig ? new ConfigCursor(spec, config, null, dependencyConfig) : new ConfigCursor(spec, config)
    this.spec = this.rootCursor.spec().spec
    this.config = this.rootCursor.config()
    this.handleObjectEdit()
    this.hasConfig = !isEmptyObject(this.spec)
  }

  dismissRec () {
    this.showRec = false
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

  async save (pkg: PackageDataEntry) {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: `Saving config...`,
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const breakages = await this.embassyApi.drySetPackageConfig({ id: pkg.manifest.id, config: this.config })

      if (!isEmptyObject(breakages.length)) {
        const { cancelled } = await wizardModal(
          this.modalController,
          this.wizardBaker.configure({
            pkg,
            breakages,
          }),
        )
        if (cancelled) return
      }

      await this.embassyApi.setPackageConfig({ id: pkg.manifest.id, config: this.config })
      this.navCtrl.back()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
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
          handler: () => {
            this.navCtrl.back()
          },
        },
      ],
    })
    await alert.present()
  }
}

