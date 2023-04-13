import { Component, Inject } from '@angular/core'
import { endWith, firstValueFrom, Subscription } from 'rxjs'
import { tuiIsString } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiDialogContext,
  TuiDialogService,
  TuiNotification,
} from '@taiga-ui/core'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getErrorMessage, isEmptyObject } from '@start9labs/shared'
import { InputSpec } from 'start-sdk/lib/config/configTypes'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { compare, Operation } from 'fast-json-patch'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages, getPackage } from 'src/app/util/get-package-data'
import { Breakages } from 'src/app/services/api/api.types'
import { InvalidService } from '../../components/form/invalid.service'
import { LoadingService } from '../loading/loading.service'
import { DependentInfo } from '../../types/dependent-info'
import { ActionButton } from '../form/form.page'

export interface PackageConfigData {
  readonly pkgId: string
  readonly dependentInfo?: DependentInfo
}

@Component({
  selector: 'app-config',
  templateUrl: './app-config.page.html',
  styleUrls: ['./app-config.page.scss'],
  providers: [InvalidService],
})
export class AppConfigPage {
  readonly pkgId = this.context.data.pkgId
  readonly dependentInfo = this.context.data.dependentInfo

  loadingError = ''
  loadingText = this.dependentInfo
    ? `Setting properties to accommodate ${this.dependentInfo.title}`
    : 'Loading Config'

  pkg?: PackageDataEntry
  spec: InputSpec = {}
  patch: Operation[] = []
  buttons: ActionButton<any>[] = [
    {
      text: 'Save',
      handler: value => this.save(value),
    },
  ]

  original!: object // only if existing config
  value?: object

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, PackageConfigData>,
    private readonly dialogs: TuiDialogService,
    private readonly alerts: TuiAlertService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly patchDb: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    try {
      this.pkg = await getPackage(this.patchDb, this.pkgId)

      if (!this.pkg?.installed?.['has-config']) {
        this.loadingError = 'This service does not have a configuration'

        return
      }

      if (this.dependentInfo) {
        const depConfig = await this.embassyApi.dryConfigureDependency({
          'dependency-id': this.pkgId,
          'dependent-id': this.dependentInfo.id,
        })

        this.original = depConfig['old-config']
        this.value = depConfig['new-config'] || this.original
        this.spec = depConfig.spec
        this.patch = compare(this.original, this.value)
      } else {
        const { config, spec } = await this.embassyApi.getPackageConfig({
          id: this.pkgId,
        })

        this.original = config
        this.value = config
        this.spec = spec
      }
    } catch (e: any) {
      const message = getErrorMessage(e)

      this.loadingError = tuiIsString(message) ? message : message.value
    } finally {
      this.loadingText = ''
    }
  }

  private async save(config: any) {
    const loader = new Subscription()

    try {
      await this.uploadFiles(config, loader)

      if (await hasCurrentDeps(this.patchDb, this.pkgId)) {
        await this.configureDeps(config, loader)
      } else {
        await this.configure(config, loader)
      }
    } catch (e: any) {
      this.showError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async uploadFiles(config: Record<string, any>, loader: Subscription) {
    loader.unsubscribe()

    // TODO: Could be nested files
    const keys = Object.keys(config).filter(key => config[key] instanceof File)
    const message = `Uploading File${keys.length > 1 ? 's' : ''}...`

    if (!keys.length) return

    loader.add(this.loader.open(message).subscribe())

    const hashes = await Promise.all(
      keys.map(key => this.embassyApi.uploadFile(config[key])),
    )
    keys.forEach((key, i) => (config[key] = hashes[i]))
  }

  private async configureDeps(
    config: Record<string, any>,
    loader: Subscription,
  ) {
    loader.unsubscribe()
    loader.add(this.loader.open('Checking dependent services...').subscribe())

    const breakages = await this.embassyApi.drySetPackageConfig({
      id: this.pkgId,
      config,
    })

    loader.unsubscribe()

    if (isEmptyObject(breakages) || (await this.approveBreakages(breakages))) {
      await this.configure(config, loader)
    }
  }

  private async configure(config: Record<string, any>, loader: Subscription) {
    loader.unsubscribe()
    loader.add(this.loader.open('Saving...').subscribe())

    await this.embassyApi.setPackageConfig({ id: this.pkgId, config })
    this.context.$implicit.complete()
  }

  private async approveBreakages(breakages: Breakages): Promise<boolean> {
    const packages = await getAllPackages(this.patchDb)
    const message =
      'As a result of this change, the following services will no longer work properly and may crash:<ul>'
    const content = `${message}${Object.keys(breakages).map(
      id => `<li><b>${packages[id].manifest.title}</b></li>`,
    )}</ul>`
    const data: TuiPromptData = { content, yes: 'Continue', no: 'Cancel' }

    return firstValueFrom(
      this.dialogs.open<boolean>(TUI_PROMPT, { data }).pipe(endWith(false)),
    )
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
