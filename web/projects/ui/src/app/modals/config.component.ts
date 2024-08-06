import { CommonModule } from '@angular/common'
import { Component, Inject, ViewChild } from '@angular/core'
import {
  ErrorService,
  getErrorMessage,
  isEmptyObject,
  LoadingService,
} from '@start9labs/shared'
import { CT, T } from '@start9labs/start-sdk'
import { TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiLoaderModule,
  TuiModeModule,
  TuiNotificationModule,
} from '@taiga-ui/core'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { compare, Operation } from 'fast-json-patch'
import { PatchDB } from 'patch-db-client'
import { endWith, firstValueFrom, Subscription } from 'rxjs'
import { ActionButton, FormComponent } from 'src/app/components/form.component'
import { InvalidService } from 'src/app/components/form/invalid.service'
import { ConfigDepComponent } from 'src/app/modals/config-dep.component'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import {
  getAllPackages,
  getManifest,
  getPackage,
} from 'src/app/util/get-package-data'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { Breakages } from 'src/app/services/api/api.types'
import { DependentInfo } from 'src/app/types/dependent-info'

export interface PackageConfigData {
  readonly pkgId: string
  readonly dependentInfo?: DependentInfo
}

@Component({
  template: `
    <tui-loader
      *ngIf="loadingText"
      size="l"
      [textContent]="loadingText"
    ></tui-loader>

    <tui-notification
      *ngIf="!loadingText && (loadingError || !pkg)"
      status="error"
    >
      <div [innerHTML]="loadingError"></div>
    </tui-notification>

    <ng-container
      *ngIf="
        !loadingText && !loadingError && pkg && (pkg | toManifest) as manifest
      "
    >
      <tui-notification *ngIf="success" status="success">
        {{ manifest.title }} has been automatically configured with recommended
        defaults. Make whatever changes you want, then click "Save".
      </tui-notification>

      <config-dep
        *ngIf="dependentInfo && value && original"
        [package]="manifest.title"
        [dep]="dependentInfo.title"
        [original]="original"
        [value]="value"
      ></config-dep>

      <tui-notification *ngIf="!manifest.hasConfig" status="warning">
        No config options for {{ manifest.title }} {{ manifest.version }}.
      </tui-notification>

      <app-form
        tuiMode="onDark"
        [spec]="spec"
        [value]="value || {}"
        [buttons]="buttons"
        [patch]="patch"
      >
        <button
          tuiButton
          appearance="flat"
          type="reset"
          [style.margin-right]="'auto'"
        >
          Reset Defaults
        </button>
      </app-form>
    </ng-container>
  `,
  styles: [
    `
      tui-notification {
        font-size: 1rem;
        margin-bottom: 1rem;
      }
    `,
  ],
  standalone: true,
  imports: [
    CommonModule,
    FormComponent,
    TuiLoaderModule,
    TuiNotificationModule,
    TuiButtonModule,
    TuiModeModule,
    ConfigDepComponent,
    UiPipeModule,
  ],
  providers: [InvalidService],
})
export class ConfigModal {
  @ViewChild(FormComponent)
  private readonly form?: FormComponent<Record<string, any>>

  readonly pkgId = this.context.data.pkgId
  readonly dependentInfo = this.context.data.dependentInfo

  loadingError = ''
  loadingText = this.dependentInfo
    ? `Setting properties to accommodate ${this.dependentInfo.title}`
    : 'Loading Config'

  pkg?: PackageDataEntry
  spec: CT.InputSpec = {}
  patch: Operation[] = []
  buttons: ActionButton<any>[] = [
    {
      text: 'Save',
      handler: value => this.save(value),
    },
  ]

  original: object | null = null
  value: object | null = null

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, PackageConfigData>,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly patchDb: PatchDB<DataModel>,
  ) {}

  get success(): boolean {
    return (
      !!this.form &&
      !this.form.form.dirty &&
      !this.original &&
      !this.pkg?.status?.configured
    )
  }

  async ngOnInit() {
    try {
      this.pkg = await getPackage(this.patchDb, this.pkgId)

      if (!this.pkg) {
        this.loadingError = 'This service does not exist'

        return
      }

      if (this.dependentInfo) {
        const depConfig = await this.embassyApi.dryConfigureDependency({
          dependencyId: this.pkgId,
          dependentId: this.dependentInfo.id,
        })

        this.original = depConfig.oldConfig
        this.value = depConfig.newConfig || this.original
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
      this.loadingError = String(getErrorMessage(e))
    } finally {
      this.loadingText = ''
    }
  }

  private async save(config: any) {
    const loader = new Subscription()

    try {
      if (hasCurrentDeps(this.pkgId, await getAllPackages(this.patchDb))) {
        await this.configureDeps(config, loader)
      } else {
        await this.configure(config, loader)
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async configureDeps(
    config: Record<string, any>,
    loader: Subscription,
  ) {
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Checking dependent services...').subscribe())

    const breakages = await this.embassyApi.drySetPackageConfig({
      id: this.pkgId,
      config,
    })

    loader.unsubscribe()
    loader.closed = false

    if (isEmptyObject(breakages) || (await this.approveBreakages(breakages))) {
      await this.configure(config, loader)
    }
  }

  private async configure(config: Record<string, any>, loader: Subscription) {
    loader.unsubscribe()
    loader.closed = false
    loader.add(this.loader.open('Saving...').subscribe())

    await this.embassyApi.setPackageConfig({ id: this.pkgId, config })
    this.context.$implicit.complete()
  }

  private async approveBreakages(breakages: T.PackageId[]): Promise<boolean> {
    const packages = await getAllPackages(this.patchDb)
    const message =
      'As a result of this change, the following services will no longer work properly and may crash:<ul>'
    const content = `${message}${breakages.map(
      id => `<li><b>${getManifest(packages[id]).title}</b></li>`,
    )}</ul>`
    const data: TuiPromptData = { content, yes: 'Continue', no: 'Cancel' }

    return firstValueFrom(
      this.dialogs.open<boolean>(TUI_PROMPT, { data }).pipe(endWith(false)),
    )
  }
}
