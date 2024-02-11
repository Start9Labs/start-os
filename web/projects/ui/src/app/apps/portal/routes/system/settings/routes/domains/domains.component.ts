import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, firstValueFrom, map } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/apps/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getCustomSpec, getStart9ToSpec } from './constants'
import { DomainsInfoComponent } from './info.component'
import { DomainsTableComponent } from './table.component'

@Component({
  template: `
    <domains-info />
    <ng-container *ngIf="domains$ | async as domains">
      <h3 class="g-title">
        Start9.to
        <button
          *ngIf="!domains.start9To.length"
          tuiButton
          size="xs"
          iconLeft="tuiIconPlus"
          (click)="claim()"
        >
          Claim
        </button>
      </h3>
      <table
        class="g-table"
        [domains]="domains.start9To"
        (delete)="delete()"
      ></table>

      <h3 class="g-title">
        Custom Domains
        <button tuiButton size="xs" iconLeft="tuiIconPlus" (click)="add()">
          Add Domain
        </button>
      </h3>
      <table
        class="g-table"
        [domains]="domains.custom"
        (delete)="delete($event.value)"
      ></table>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiButtonModule,
    DomainsTableComponent,
    DomainsInfoComponent,
  ],
})
export class SettingsDomainsComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)

  readonly domains$ = this.patch.watch$('server-info', 'network').pipe(
    map(network => {
      const start9ToSubdomain = network.start9ToSubdomain
      const start9To = !start9ToSubdomain
        ? []
        : [
            {
              ...start9ToSubdomain,
              value: `${start9ToSubdomain.value}.start9.to`,
              provider: 'Start9',
            },
          ]

      return { start9To, custom: network.domains }
    }),
  )

  delete(hostname?: string) {
    this.dialogs
      .open(TUI_PROMPT, {
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
      this.patch.watch$('server-info', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'Custom Domain',
      data: {
        spec: await getCustomSpec(proxies),
        buttons: [
          {
            text: 'Manage proxies',
            link: '/portal/system/settings/proxies',
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
      this.patch.watch$('server-info', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'start9.to',
      data: {
        spec: await getStart9ToSpec(proxies),
        buttons: [
          {
            text: 'Manage proxies',
            link: '/portal/system/settings/proxies',
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

  private getNetworkStrategy(strategy: any) {
    const { ipStrategy, proxyStrategy = {} } = strategy.unionValueKey
    const { unionSelectKey, unionValueKey = {} } = proxyStrategy
    const proxyId = unionSelectKey === 'primary' ? null : unionValueKey.proxyId

    return strategy.unionSelectKey === 'local' ? { ipStrategy } : { proxyId }
  }

  private async deleteDomain(hostname?: string) {
    const loader = this.loader.open('Deleting...').subscribe()

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

  private async claimDomain({ strategy }: any): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()
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

  private async save({ provider, strategy, hostname }: any): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()
    const name = provider.unionSelectKey

    try {
      await this.api.addDomain({
        hostname,
        networkStrategy: this.getNetworkStrategy(strategy),
        provider: {
          name,
          username: name === 'start9' ? null : provider.unionValueKey.username,
          password: name === 'start9' ? null : provider.unionValueKey.password,
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
