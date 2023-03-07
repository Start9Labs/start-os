import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { combineLatest, filter, first, map, switchMap } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DomainSpec, domainSpec } from './domain.const'
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

  readonly domains$ = this.connectionService.connected$.pipe(
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
    const options: Partial<TuiDialogOptions<FormContext<DomainSpec>>> = {
      label: 'Custom Domain',
      data: {
        spec: await domainSpec.build({} as any),
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

  presentAlertClaimStart9MeDomain() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Claim your start9.me domain?',
          yes: 'Claim',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.claimStart9MeDomain())
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

  private async claimStart9MeDomain(): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.claimStart9MeDomain({})
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(value: DomainSpec): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addDomain(value)
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
