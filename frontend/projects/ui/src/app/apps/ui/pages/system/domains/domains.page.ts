import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { combineLatest, filter, first, map, switchMap } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  start9MeSpec,
  Start9MeSpec,
  customSpec,
  CustomSpec,
} from './domain.const'
import { ConnectionService } from 'src/app/services/connection.service'
import { FormContext, FormPage } from '../../../modals/form/form.page'
import { getClearnetAddress } from 'src/app/util/clearnetAddress'

@Component({
  selector: 'domains',
  templateUrl: 'domains.page.html',
  styleUrls: ['domains.page.scss'],
})
export class DomainsPage {
  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/domains'

  readonly server$ = this.patch.watch$('server-info')
  readonly pkgs$ = this.patch.watch$('package-data').pipe(first())

  readonly domains$ = this.connectionService.websocketConnected$.pipe(
    filter(Boolean),
    switchMap(() =>
      combineLatest([this.server$, this.pkgs$]).pipe(
        map(([{ ui, network }, packageData]) => {
          const start9MeSubdomain = network.start9MeSubdomain
          const start9Me = !start9MeSubdomain
            ? null
            : {
                value: `${start9MeSubdomain.value}.start9.me`,
                createdAt: start9MeSubdomain.createdAt,
                provider: 'Start9',
                networkStrategy: start9MeSubdomain.networkStrategy,
                ipStrategy: start9MeSubdomain.ipStrategy,
                usedBy: usedBy(
                  start9MeSubdomain.value,
                  getClearnetAddress('https', ui.domainInfo),
                  packageData,
                ),
              }
          const custom = network.domains.map(domain => ({
            value: domain.value,
            createdAt: domain.createdAt,
            provider: domain.provider,
            networkStrategy: domain.networkStrategy,
            ipStrategy: domain.ipStrategy,
            usedBy: usedBy(
              domain.value,
              getClearnetAddress('https', ui.domainInfo),
              packageData,
            ),
          }))

          return { start9Me, custom }
        }),
      ),
    ),
  )

  constructor(
    private readonly errorService: ErrorService,
    private readonly dialogs: TuiDialogService,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async presentModalAdd() {
    const options: Partial<TuiDialogOptions<FormContext<CustomSpec>>> = {
      label: 'Custom Domain',
      data: {
        spec: await customSpec.build({} as any),
        buttons: [
          {
            text: 'Save',
            handler: async value => this.save(value),
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  async presentModalClaimStart9Me() {
    const options: Partial<TuiDialogOptions<FormContext<Start9MeSpec>>> = {
      label: 'start9.me',
      data: {
        spec: await start9MeSpec.build({} as any),
        buttons: [
          {
            text: 'Save',
            handler: async value => this.claimStart9MeDomain(value),
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  presentAlertDelete(hostname: string) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Delete domain?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.delete(hostname))
  }

  presentAlertDeleteStart9Me() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Delete start9.me domain?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.deleteStart9MeDomain())
  }

  presentAlertUsedBy(domain: string, usedBy: string[]) {
    this.dialogs
      .open(
        `${domain} is currently being used by:<ul>${usedBy.map(
          u => `<li>${u}</li>`,
        )}</ul>`,
        {
          label: 'Used by',
          size: 's',
        },
      )
      .subscribe()
  }

  private async claimStart9MeDomain(value: Start9MeSpec): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const networkStrategy = value.strategy.unionSelectKey

    try {
      await this.api.claimStart9MeDomain({
        networkStrategy,
        ipStrategy:
          networkStrategy === 'router' ? value.strategy.unionValueKey.ip : null,
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(value: CustomSpec): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const networkStrategy = value.strategy.unionSelectKey
    const providerName = value.provider.unionSelectKey

    try {
      await this.api.addDomain({
        hostname: value.hostname,
        provider: {
          name: providerName,
          username:
            providerName === 'start9'
              ? null
              : value.provider.unionValueKey.username,
          password:
            providerName === 'start9'
              ? null
              : value.provider.unionValueKey.password,
        },
        networkStrategy,
        ipStrategy:
          networkStrategy === 'router' ? value.strategy.unionValueKey.ip : null,
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async delete(hostname: string): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteDomain({ hostname })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async deleteStart9MeDomain(): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteStart9MeDomain({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

function usedBy(
  domain: string,
  serverUi: string | null,
  pkgs: DataModel['package-data'],
): string[] {
  const list = []
  if (serverUi && serverUi.includes(domain)) list.push('StartOS Web Interface')
  return list.concat(
    Object.values(pkgs)
      .filter(pkg =>
        Object.values(pkg.installed?.['address-info'] || {}).some(ai =>
          ai.addresses.some(a => a.includes(domain)),
        ),
      )
      .map(pkg => pkg.manifest.title),
  )
}
