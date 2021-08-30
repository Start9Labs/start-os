import { Component, Input, ViewChild } from '@angular/core'
import { AlertController, ModalController, IonContent, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { isEmptyObject, Recommendation } from 'src/app/util/misc.util'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { FormGroup } from '@angular/forms'
import { FormService } from 'src/app/services/form.service'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage {
  @Input() pkgId: string
  loadingText: string | undefined
  configSpec: ConfigSpec
  configForm: FormGroup
  current: object
  hasConfig = false

  rec: Recommendation | null = null
  showRec = true
  openRec = false

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly wizardBaker: WizardBaker,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
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
      this.loadingText = undefined
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  setConfig (spec: ConfigSpec, config: object, depConfig?: object) {
    this.configSpec = spec
    this.current = config
    this.hasConfig = !isEmptyObject(config)
    this.configForm = this.formService.createForm(spec, { ...config, ...depConfig })
    this.configForm.markAllAsTouched()

    if (depConfig) {
      this.markDirtyRecursive(this.configForm, depConfig)
    }
  }

  markDirtyRecursive (group: FormGroup, config: object) {
    Object.keys(config).forEach(key => {
      const next = group.get(key)
      if (!next) throw new Error('Dependency config not compatible with service version. Please contact support')
      const newVal = config[key]
      // check if val is an object
      if (newVal && typeof newVal === 'object' && !Array.isArray(newVal)) {
        this.markDirtyRecursive(next as FormGroup, newVal)
      } else {
        let val1 = group.get(key).value
        let val2 = config[key]
        if (Array.isArray(newVal)) {
          val1 = JSON.stringify(val1)
          val2 = JSON.stringify(val2)
        }
        if (val1 != val2) next.markAsDirty()
      }
    })
  }

  resetDefaults () {
    this.configForm = this.formService.createForm(this.configSpec)
    this.markDirtyRecursive(this.configForm, this.current)
  }

  dismissRec () {
    this.showRec = false
  }

  async dismiss () {
    if (this.configForm?.dirty) {
      await this.presentAlertUnsaved()
    } else {
      this.modalCtrl.dismiss()
    }
  }

  async save (pkg: PackageDataEntry) {
    if (this.configForm.invalid) {
      document.getElementsByClassName('validation-error')[0].parentElement.parentElement.scrollIntoView({ behavior: 'smooth' })
      return
    }

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: `Saving config...`,
      cssClass: 'loader',
    })
    await loader.present()

    const config = this.configForm.value

    try {
      const breakages = await this.embassyApi.drySetPackageConfig({
        id: pkg.manifest.id,
        config,
      })

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

      await this.embassyApi.setPackageConfig({
        id: pkg.manifest.id,
        config,
      })
      this.modalCtrl.dismiss()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertUnsaved () {
    const alert = await this.alertCtrl.create({
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
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }
}

