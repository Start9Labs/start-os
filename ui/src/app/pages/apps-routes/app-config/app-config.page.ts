import { Component, ViewChild } from '@angular/core'
import { NavController, AlertController, ModalController, PopoverController, IonContent } from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { isEmptyObject } from 'src/app/util/misc.util'
import { LoaderService } from 'src/app/services/loader.service'
import { TrackingModalController } from 'src/app/services/tracking-modal-controller.service'
import { from, fromEvent, of, Subscription } from 'rxjs'
import { catchError, concatMap, map, take, tap } from 'rxjs/operators'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { InstalledPackageDataEntry, PackageState } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

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

  pkg: InstalledPackageDataEntry
  hasConfig = false

  backButtonDefense = false
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
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly alertCtrl: AlertController,
    private readonly modalController: ModalController,
    private readonly trackingModalCtrl: TrackingModalController,
    private readonly popoverController: PopoverController,
    private readonly patch: PatchDbModel,
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
      this.patch.watch$('package-data', pkgId, 'installed')
      .pipe(
        tap(pkg => this.pkg = pkg),
        tap(() => this.loadingText = 'Fetching config spec...'),
        concatMap(() => this.apiService.getPackageConfig({ id: pkgId })),
        concatMap(({ spec, config }) => {
          const rec = history.state && history.state.configRecommendation as Recommendation
          if (rec) {
            this.loadingText = `Setting properties to accommodate ${rec.dependentTitle}...`
            return from(this.apiService.dryConfigureDependency({ 'dependency-id': pkgId, 'dependent-id': rec.dependentId }))
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

  async save (pkg: InstalledPackageDataEntry) {
    return this.loader.of({
      message: `Saving config...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      const breakages = await this.apiService.drySetPackageConfig({ id: pkg.manifest.id, config: this.config })

      if (!isEmptyObject(breakages.length)) {
        const { cancelled } = await wizardModal(
          this.modalController,
          this.wizardBaker.configure({
            pkg,
            breakages,
          }),
        )
        if (cancelled) return { skip: true }
      }

      return this.apiService.setPackageConfig({ id: pkg.manifest.id, config: this.config })
        .then(() => ({ skip: false }))
    })
    .then(({ skip }) => {
      if (skip) return
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

