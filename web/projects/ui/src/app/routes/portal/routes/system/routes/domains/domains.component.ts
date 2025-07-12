import { TUI_CONFIRM } from '@taiga-ui/kit'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService, TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { filter, firstValueFrom, map } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getCustomSpec, getStart9ToSpec } from './constants'
import { DomainsInfoComponent } from './info.component'
import { DomainsTableComponent } from './table.component'

@Component({
  template: `
    <domains-info />
    @if (domains$ | async; as domains) {
      <h3 class="g-title">
        Start9.to
        @if (!domains.start9To.length) {
          <button tuiButton size="xs" iconStart="@tui.plus" (click)="claim()">
            Claim
          </button>
        }
      </h3>
      <table
        class="g-table"
        [domains]="domains.start9To"
        (delete)="delete()"
      ></table>
      <h3 class="g-title">
        Custom Domains
        <button tuiButton size="xs" iconStart="@tui.plus" (click)="add()">
          Add Domain
        </button>
      </h3>
      <table
        class="g-table"
        [domains]="domains.custom"
        (delete)="delete($event.value)"
      ></table>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    DomainsTableComponent,
    DomainsInfoComponent,
  ],
})
export default class SystemDomainsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)

  private readonly start9To$ = this.patch.watch$(
    'serverInfo',
    'network',
    'start9To',
  )

  readonly domains$ = this.patch.watch$('serverInfo', 'network', 'domains')

  delete(hostname?: string) {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Confirm',
        size: 's',
        data: {
          content: `Delete ${hostname || 'start9.to'} domain?`,
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.deleteDomain(hostname))
  }

  async add() {
    const proxies = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'Custom Domain',
      data: {
        spec: await getCustomSpec(proxies),
        buttons: [
          {
            text: 'Manage proxies',
            link: '/system/proxies',
          },
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    }

    this.formDialog.open(FormComponent, options)
  }

  async claim() {
    const proxies = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'start9.to',
      data: {
        spec: await getStart9ToSpec(proxies),
        buttons: [
          {
            text: 'Manage proxies',
            link: '/system/proxies',
          },
          {
            text: 'Save',
            handler: async value => this.claimDomain(value),
          },
        ],
      },
    }

    this.formDialog.open(FormComponent, options)
  }
  // @TODO 041 figure out how to get types here
  private getNetworkStrategy(strategy: any) {
    return strategy.selection === 'local'
      ? { ipStrategy: strategy.value.ipStrategy }
      : { proxy: strategy.value.proxyId }
  }

  private async deleteDomain(hostname?: string) {
    const loader = this.loader.open('Deleting').subscribe()

    try {
      if (hostname) {
        await this.api.deleteDomain({ hostname })
      } else {
        await this.api.deleteStart9ToDomain({})
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
  // @TODO 041 figure out how to get types here
  private async claimDomain({ strategy }: any): Promise<boolean> {
    const loader = this.loader.open('Saving').subscribe()
    const networkStrategy = this.getNetworkStrategy(strategy)

    try {
      await this.api.claimStart9ToDomain({ networkStrategy })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
  // @TODO 041 figure out how to get types here
  private async save({ provider, strategy, hostname }: any): Promise<boolean> {
    const loader = this.loader.open('Saving').subscribe()
    const name = provider.selection

    try {
      await this.api.addDomain({
        hostname,
        networkStrategy: this.getNetworkStrategy(strategy),
        provider: {
          name,
          username: name === 'start9' ? null : provider.value.username,
          password: name === 'start9' ? null : provider.value.password,
        },
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
