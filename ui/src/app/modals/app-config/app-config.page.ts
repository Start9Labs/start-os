import { Component, Input, ViewChild } from '@angular/core'
import { AlertController, ModalController, IonContent, LoadingController, IonicSafeString } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DependentInfo, isEmptyObject } from 'src/app/util/misc.util'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService, getErrorMessage } from 'src/app/services/error-toast.service'
import { FormGroup } from '@angular/forms'
import { convertValuesRecursive, FormService } from 'src/app/services/form.service'
import { compare, Operation } from 'fast-json-patch'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage {
  @ViewChild(IonContent) content: IonContent
  @Input() pkgId: string
  @Input() dependentInfo?: DependentInfo
  pkg: PackageDataEntry
  loadingText: string | undefined
  configSpec: ConfigSpec
  configForm: FormGroup
  original: object
  hasConfig = false
  saving = false
  loadingError: string | IonicSafeString

  constructor (
    private readonly wizardBaker: WizardBaker,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
    private readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    this.pkg = this.patch.getData()['package-data'][this.pkgId]
    this.hasConfig = !!this.pkg.manifest.config

    if (!this.hasConfig) return

    try {
      let oldConfig: object
      let newConfig: object
      let spec: ConfigSpec
      let patch: Operation[]
      if (this.dependentInfo) {
        this.loadingText = `Setting properties to accommodate ${this.dependentInfo.title}`
        const { 'old-config': oc, 'new-config': nc, spec: s } = await this.embassyApi.dryConfigureDependency({ 'dependency-id': this.pkgId, 'dependent-id': this.dependentInfo.id })
        oldConfig = oc
        newConfig = nc
        spec = s
        patch = compare(oldConfig, newConfig)
      } else {
        this.loadingText = 'Loading Config'
        const { config: c, spec: s } = await this.embassyApi.getPackageConfig({ id: this.pkgId })
        oldConfig = c
        spec = s
      }

      this.original = oldConfig
      this.configSpec = spec
      this.configForm = this.formService.createForm(spec, newConfig || oldConfig)
      this.configForm.markAllAsTouched()

      if (patch) {
        this.markDirty(patch)
      }
    } catch (e) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loadingText = undefined
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  resetDefaults () {
    this.configForm = this.formService.createForm(this.configSpec)
    const patch = compare(this.original, this.configForm.value)
    this.markDirty(patch)
  }

  async dismiss () {
    if (this.configForm?.dirty) {
      await this.presentAlertUnsaved()
    } else {
      this.modalCtrl.dismiss()
    }
  }

  async save () {
    convertValuesRecursive(this.configSpec, this.configForm)

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

    this.saving = true

    try {
      const config = this.configForm.value

      const breakages = await this.embassyApi.drySetPackageConfig({
        id: this.pkgId,
        config,
      })

      if (!isEmptyObject(breakages.length)) {
        const { cancelled } = await wizardModal(
          this.modalCtrl,
          this.wizardBaker.configure({
            pkg: this.pkg,
            breakages,
          }),
        )
        if (cancelled) return
      }

      await this.embassyApi.setPackageConfig({
        id: this.pkgId,
        config,
      })
      this.modalCtrl.dismiss()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.saving = false
      loader.dismiss()
    }
  }

  private markDirty (patch: Operation[]) {
    patch.forEach(op => {
      const arrPath = op.path.substring(1)
      .split('/')
      .map(node => {
        const num = Number(node)
        return isNaN(num) ? node : num
      })

      if (op.op !== 'remove') this.configForm.get(arrPath).markAsDirty()

      if (typeof arrPath[arrPath.length - 1] === 'number') {
        const prevPath = arrPath.slice(0, arrPath.length - 1)
        this.configForm.get(prevPath).markAsDirty()
      }
    })
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

