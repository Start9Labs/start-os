import { Component, Inject } from '@angular/core'
import { endWith, firstValueFrom, Subscription } from 'rxjs'
import { tuiIsString, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiDialogContext,
  TuiDialogService,
  TuiNotification,
} from '@taiga-ui/core'
import { TUI_PROMPT, TuiDialogFormService } from '@taiga-ui/kit'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getErrorMessage, isEmptyObject, isObject } from '@start9labs/shared'
import { InputSpec } from 'start-sdk/lib/config/configTypes'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { UntypedFormGroup } from '@angular/forms'
import { FormService } from 'src/app/services/form.service'
import { compare, getValueByPointer, Operation } from 'fast-json-patch'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages, getPackage } from 'src/app/util/get-package-data'
import { Breakages } from 'src/app/services/api/api.types'
import { InvalidService } from '../../components/form/invalid.service'
import { LoadingService } from '../loading/loading.service'
import { DependentInfo } from '../../types/dependent-info'

export interface PackageConfigData {
  readonly pkgId: string
  readonly dependentInfo?: DependentInfo
}

// TODO: Refactor to use form.page.ts inside
@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
  providers: [InvalidService],
})
export class AppConfigPage {
  readonly pkgId = this.context.data.pkgId
  readonly dependentInfo = this.context.data.dependentInfo

  pkg!: PackageDataEntry

  loadingText = ''

  configSpec?: InputSpec
  configForm?: UntypedFormGroup

  original?: object // only if existing config
  diff?: string[] // only if dependent info

  loading = true
  loadingError = ''

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    readonly context: TuiDialogContext<void, PackageConfigData>,
    private readonly dialogFormService: TuiDialogFormService,
    private readonly dialogs: TuiDialogService,
    private readonly alerts: TuiAlertService,
    private readonly invalidService: InvalidService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly formService: FormService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    try {
      const pkg = await getPackage(this.patch, this.pkgId)

      if (!pkg?.installed?.['has-config']) return

      this.pkg = pkg

      let newConfig: object | undefined
      let patch: Operation[] | undefined

      if (this.dependentInfo) {
        this.loadingText = `Setting properties to accommodate ${this.dependentInfo.title}`

        const depConfig = await this.embassyApi.dryConfigureDependency({
          'dependency-id': this.pkgId,
          'dependent-id': this.dependentInfo.id,
        })

        this.original = depConfig['old-config']
        this.configSpec = depConfig.spec
        newConfig = depConfig['new-config']

        patch = compare(this.original, newConfig)
      } else {
        this.loadingText = 'Loading Config'

        const { config, spec } = await this.embassyApi.getPackageConfig({
          id: this.pkgId,
        })

        this.original = config
        this.configSpec = spec
      }

      this.configForm = this.formService.createForm(
        this.configSpec!,
        newConfig || this.original,
      )

      if (patch) {
        this.diff = this.getDiff(patch)
        this.markDirty(patch)
      }
    } catch (e: any) {
      const message = getErrorMessage(e)

      this.loadingError = tuiIsString(message) ? message : message.value
    } finally {
      this.loading = false
    }
  }

  resetDefaults() {
    this.configForm = this.formService.createForm(this.configSpec!)
    this.markDirty(compare(this.original || {}, this.configForm.value))
  }

  markAsDirty() {
    this.dialogFormService.markAsDirty()
  }

  save() {
    tuiMarkControlAsTouchedAndValidate(this.configForm!)

    if (this.configForm?.valid) {
      this.tryConfigure(this.configForm.value)
    } else {
      this.invalidService.scrollIntoView()
    }
  }

  async tryConfigure(config: any) {
    const loader = new Subscription()
    const fileKeys = Object.keys(config).filter(
      key => config[key] instanceof File,
    )

    if (fileKeys.length) {
      loader.add(
        this.loader
          .open(`Uploading File${fileKeys.length > 1 ? 's' : ''}...`)
          .subscribe(),
      )

      try {
        const hashes = await Promise.all(
          fileKeys.map(key => this.embassyApi.uploadFile(config[key])),
        )
        fileKeys.forEach((key, i) => (config[key] = hashes[i]))
      } catch (e: any) {
        this.showError(e)
      } finally {
        loader.unsubscribe()
        return
      }
    }

    if (await hasCurrentDeps(this.patch, this.pkgId)) {
      this.dryConfigure(config, loader)
    } else {
      this.configure(config, loader)
    }
  }

  private async dryConfigure(
    config: Record<string, any>,
    loader: Subscription,
  ) {
    loader.unsubscribe()
    loader.add(this.loader.open('Checking dependent services...').subscribe())

    try {
      const breakages = await this.embassyApi.drySetPackageConfig({
        id: this.pkgId,
        config,
      })

      if (isEmptyObject(breakages)) {
        this.configure(config, loader)
      } else {
        loader.unsubscribe()

        if (await this.presentAlertBreakages(breakages)) {
          this.configure(config, loader)
        }
      }
    } catch (e: any) {
      this.showError(e)
      loader.unsubscribe()
    }
  }

  private async configure(config: Record<string, any>, loader: Subscription) {
    loader.unsubscribe()
    loader.add(this.loader.open('Saving...').subscribe())

    try {
      await this.embassyApi.setPackageConfig({
        id: this.pkgId,
        config,
      })
      this.context.$implicit.complete()
    } catch (e: any) {
      this.showError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async presentAlertBreakages(breakages: Breakages): Promise<boolean> {
    const localPkgs = await getAllPackages(this.patch)
    const content = `As a result of this change, the following services will no longer work properly and may crash:<ul>}${Object.keys(
      breakages,
    ).map(id => `<li><b>${localPkgs[id].manifest.title}</b></li>`)}</ul>`

    return firstValueFrom(
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          data: {
            content,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(endWith(false)),
    )
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
    this.markAsDirty()
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

  private showError(e: any) {
    const message = getErrorMessage(e)

    this.alerts
      .open(tuiIsString(message) ? message : message.value, {
        status: TuiNotification.Error,
        autoClose: false,
        label: 'Error',
      })
      .subscribe()
  }
}
