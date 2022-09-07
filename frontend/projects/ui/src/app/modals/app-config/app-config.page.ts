import { Component, Input } from '@angular/core'
import {
  AlertController,
  ModalController,
  LoadingController,
  IonicSafeString,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  ErrorToastService,
  getErrorMessage,
  isEmptyObject,
  isObject,
} from '@start9labs/shared'
import { DependentInfo } from 'src/app/types/dependent-info'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { UntypedFormGroup } from '@angular/forms'
import {
  convertValuesRecursive,
  FormService,
} from 'src/app/services/form.service'
import { compare, Operation, getValueByPointer } from 'fast-json-patch'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages, getPackage } from 'src/app/util/get-package-data'
import { Breakages } from 'src/app/services/api/api.types'

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
})
export class AppConfigPage {
  @Input() pkgId!: string

  @Input() dependentInfo?: DependentInfo

  pkg!: PackageDataEntry
  loadingText = ''

  configSpec?: ConfigSpec
  configForm?: UntypedFormGroup

  original?: object // only if existing config
  diff?: string[] // only if dependent info

  loading = true
  hasNewOptions = false
  saving = false
  loadingError: string | IonicSafeString = ''

  constructor(
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    try {
      this.pkg = await getPackage(this.patch, this.pkgId)

      if (!this.pkg.manifest.config) return

      let newConfig: object | undefined
      let patch: Operation[] | undefined

      if (this.dependentInfo) {
        this.loadingText = `Setting properties to accommodate ${this.dependentInfo.title}`
        const {
          'old-config': oc,
          'new-config': nc,
          spec: s,
        } = await this.embassyApi.dryConfigureDependency({
          'dependency-id': this.pkgId,
          'dependent-id': this.dependentInfo.id,
        })
        this.original = oc
        newConfig = nc
        this.configSpec = s
        patch = compare(this.original, newConfig)
      } else {
        this.loadingText = 'Loading Config'
        const { config: c, spec: s } = await this.embassyApi.getPackageConfig({
          id: this.pkgId,
        })
        this.original = c
        this.configSpec = s
      }

      this.configForm = this.formService.createForm(
        this.configSpec,
        newConfig || this.original,
      )
      this.configForm.markAllAsTouched()

      if (patch) {
        this.diff = this.getDiff(patch)
        this.markDirty(patch)
      }
    } catch (e: any) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loading = false
    }
  }

  resetDefaults() {
    this.configForm = this.formService.createForm(this.configSpec!)
    const patch = compare(this.original || {}, this.configForm.value)
    this.markDirty(patch)
  }

  async dismiss() {
    if (this.configForm?.dirty) {
      this.presentAlertUnsaved()
    } else {
      this.modalCtrl.dismiss()
    }
  }

  async tryConfigure() {
    convertValuesRecursive(this.configSpec!, this.configForm!)

    if (this.configForm!.invalid) {
      document
        .getElementsByClassName('validation-error')[0]
        ?.scrollIntoView({ behavior: 'smooth' })
      return
    }

    this.saving = true

    if (hasCurrentDeps(this.pkg)) {
      this.dryConfigure()
    } else {
      this.configure()
    }
  }

  private async dryConfigure() {
    const loader = await this.loadingCtrl.create({
      message: 'Checking dependent services...',
    })
    await loader.present()

    try {
      const breakages = await this.embassyApi.drySetPackageConfig({
        id: this.pkgId,
        config: this.configForm!.value,
      })

      if (isEmptyObject(breakages)) {
        this.configure(loader)
      } else {
        await loader.dismiss()
        const proceed = await this.presentAlertBreakages(breakages)
        if (proceed) {
          this.configure()
        } else {
          this.saving = false
        }
      }
    } catch (e: any) {
      this.errToast.present(e)
      this.saving = false
      loader.dismiss()
    }
  }

  private async configure(loader?: HTMLIonLoadingElement) {
    const message = 'Saving...'
    if (loader) {
      loader.message = message
    } else {
      loader = await this.loadingCtrl.create({ message })
      await loader.present()
    }

    try {
      await this.embassyApi.setPackageConfig({
        id: this.pkgId,
        config: this.configForm!.value,
      })
      this.modalCtrl.dismiss()
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.saving = false
      loader.dismiss()
    }
  }

  private async presentAlertBreakages(breakages: Breakages): Promise<boolean> {
    let message: string =
      'As a result of this change, the following services will no longer work properly and may crash:<ul>'
    const localPkgs = await getAllPackages(this.patch)
    const bullets = Object.keys(breakages).map(id => {
      const title = localPkgs[id].manifest.title
      return `<li><b>${title}</b></li>`
    })
    message = `${message}${bullets}</ul>`

    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => {
              resolve(false)
            },
          },
          {
            text: 'Continue',
            handler: () => {
              resolve(true)
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    })
  }

  private getDiff(patch: Operation[]): string[] {
    return patch.map(op => {
      let message: string
      switch (op.op) {
        case 'add':
          message = `Added ${this.getNewValue(op.value)}`
          break
        case 'remove':
          message = `Removed ${this.getOldValue(op.path)}`
          break
        case 'replace':
          message = `Changed from ${this.getOldValue(
            op.path,
          )} to ${this.getNewValue(op.value)}`
          break
        default:
          message = `Unknown operation`
      }

      let displayPath: string

      const arrPath = op.path
        .substring(1)
        .split('/')
        .map(node => {
          const num = Number(node)
          return isNaN(num) ? node : num
        })

      if (typeof arrPath[arrPath.length - 1] === 'number') {
        arrPath.pop()
      }

      displayPath = arrPath.join(' &rarr; ')

      return `${displayPath}: ${message}`
    })
  }

  private getOldValue(path: any): string {
    const val = getValueByPointer(this.original, path)
    if (['string', 'number', 'boolean'].includes(typeof val)) {
      return val
    } else if (isObject(val)) {
      return 'entry'
    } else {
      return 'list'
    }
  }

  private getNewValue(val: any): string {
    if (['string', 'number', 'boolean'].includes(typeof val)) {
      return val
    } else if (isObject(val)) {
      return 'new entry'
    } else {
      return 'new list'
    }
  }

  private markDirty(patch: Operation[]) {
    patch.forEach(op => {
      const arrPath = op.path
        .substring(1)
        .split('/')
        .map(node => {
          const num = Number(node)
          return isNaN(num) ? node : num
        })

      if (op.op !== 'remove') this.configForm!.get(arrPath)?.markAsDirty()

      if (typeof arrPath[arrPath.length - 1] === 'number') {
        const prevPath = arrPath.slice(0, arrPath.length - 1)
        this.configForm!.get(prevPath)?.markAsDirty()
      }
    })
  }

  private async presentAlertUnsaved() {
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
