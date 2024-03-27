import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import {
  FormComponent,
  FormContext,
} from 'src/app/apps/portal/components/form.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ProxiesTableComponent } from './table.component'
import { ProxiesInfoComponent } from './info.component'
import { wireguardSpec, WireguardSpec } from './constants'

@Component({
  template: `
    <proxies-info />
    <h3 class="g-title">
      Proxies
      <button tuiButton size="xs" iconLeft="tuiIconPlus" (click)="add()">
        Add Proxy
      </button>
    </h3>
    <table class="g-table" [proxies]="(proxies$ | async) || []"></table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiButtonModule,
    ProxiesInfoComponent,
    ProxiesTableComponent,
  ],
})
export class SettingsProxiesComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)

  readonly proxies$ = inject(PatchDB<DataModel>).watch$(
    'serverInfo',
    'network',
    'proxies',
  )

  async add() {
    const options: Partial<TuiDialogOptions<FormContext<WireguardSpec>>> = {
      label: 'Add Proxy',
      data: {
        spec: await wireguardSpec.build({} as any),
        buttons: [
          {
            text: 'Save',
            handler: value => this.save(value).then(() => true),
          },
        ],
      },
    }

    this.formDialog.open(FormComponent, options)
  }

  private async save({ name, config }: WireguardSpec): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addProxy({ name, config: config?.filePath || '' })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
