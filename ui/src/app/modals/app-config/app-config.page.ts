import { Component, Input, ViewChild } from '@angular/core'
import { AlertController, ModalController, IonContent, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { isEmptyObject, Recommendation } from 'src/app/util/misc.util'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { AppConfigObjectPage } from 'src/app/modals/app-config-object/app-config-object.page'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage {
  @Input() pkgId: string
  loadingText: string | undefined

  hasConfig = false

  rec: Recommendation | null = null
  showRec = true
  openRec = false

  invalid: string
  edited: boolean
  rootCursor: ConfigCursor<'object'>
  @ViewChild(IonContent) content: IonContent

  rootPage = AppConfigObjectPage

  constructor (
    private readonly wizardBaker: WizardBaker,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    public readonly patch: PatchDbService,

  ) { }

  async ngOnInit () {
    const rec = history.state?.configRecommendation as Recommendation

    try {
      this.loadingText = 'Loading Config'
      const { spec, config } = await this.embassyApi.getPackageConfig({ id: this.pkgId })

      let depConfig: object
      if (rec) {
        this.loadingText = `Setting properties to accommodate ${rec.dependentTitle}...`
        depConfig = await this.embassyApi.dryConfigureDependency({ 'dependency-id': this.pkgId, 'dependent-id': rec.dependentId })
      }
      this.setConfig(spec, config, depConfig)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loadingText = ''
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  setConfig (spec: ConfigSpec, config: object, dependencyConfig?: object) {
    this.rootCursor = new ConfigCursor(spec, config, null, dependencyConfig)
    this.handleObjectEdit()
    this.hasConfig = !isEmptyObject(this.rootCursor.spec().spec)
  }

  dismissRec () {
    this.showRec = false
  }

  async cancel () {
    if (this.edited) {
      await this.presentAlertUnsaved()
    } else {
      this.modalCtrl.dismiss()
    }
  }

  async save (pkg: PackageDataEntry) {
    const config = this.rootCursor.config()

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: `Saving config...`,
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const breakages = await this.embassyApi.drySetPackageConfig({ id: pkg.manifest.id, config })

      if (!isEmptyObject(breakages.length)) {
        const { cancelled } = await wizardModal(
          this.modalCtrl,
          this.wizardBaker.configure({
            pkg,
            breakages,
          }),
        )
        if (cancelled) return
      }

      await this.embassyApi.setPackageConfig({ id: pkg.manifest.id, config })
      this.modalCtrl.dismiss()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  handleObjectEdit () {
    this.edited = this.rootCursor.isEdited()
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
            this.modalCtrl.dismiss()
          },
        },
      ],
    })
    await alert.present()
  }
}

