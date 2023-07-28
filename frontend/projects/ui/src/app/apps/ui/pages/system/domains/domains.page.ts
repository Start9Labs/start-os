import { Component } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter, firstValueFrom, map } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, Domain } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormContext, FormPage } from '../../../modals/form/form.page'
import { getCustomSpec, getStart9MeSpec } from './domain.const'

@Component({
  selector: 'domains',
  templateUrl: 'domains.page.html',
  styleUrls: ['domains.page.scss'],
})
export class DomainsPage {
  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/domains'

  readonly domains$ = this.patch.watch$('server-info', 'network').pipe(
    map(network => {
      const start9MeSubdomain = network.start9MeSubdomain
      const start9Me = !start9MeSubdomain
        ? null
        : {
            ...start9MeSubdomain,
            value: `${start9MeSubdomain.value}.start9.me`,
            provider: 'Start9',
          }

      return { start9Me, custom: network.domains }
    }),
  )

  constructor(
    private readonly errorService: ErrorService,
    private readonly dialogs: TuiDialogService,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async presentModalAdd() {
    const proxies = await firstValueFrom(
      this.patch.watch$('server-info', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'Custom Domain',
      data: {
        spec: await getCustomSpec(proxies),
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
    const proxies = await firstValueFrom(
      this.patch.watch$('server-info', 'network', 'proxies'),
    )

    const options: Partial<TuiDialogOptions<FormContext<any>>> = {
      label: 'start9.me',
      data: {
        spec: await getStart9MeSpec(proxies),
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

  presentAlertUsedBy(domain: string, usedBy: Domain['usedBy']) {
    this.dialogs
      .open(
        `${domain} is currently being used by:<ul>${usedBy.map(u =>
          u.interfaces.map(i => `<li>${u.service.title} - ${i.title}</li>`),
        )}</ul>`,
        {
          label: 'Used by',
          size: 's',
        },
      )
      .subscribe()
  }

  private async claimStart9MeDomain(value: any): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const strategy = value.strategy.unionValueKey

    const networkStrategy =
      value.strategy.unionSelectKey === 'router'
        ? { ipStrategy: strategy.ipStrategy }
        : {
            proxyId:
              strategy.proxyStrategy.unionSelectKey === 'primary'
                ? null
                : strategy.proxyStrategy.unionValueKey.proxyId,
          }

    try {
      await this.api.claimStart9MeDomain({ networkStrategy })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async save(value: any): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    const providerName = value.provider.unionSelectKey

    const strategy = value.strategy.unionValueKey

    const networkStrategy =
      value.strategy.unionSelectKey === 'router'
        ? { ipStrategy: strategy.ipStrategy }
        : {
            proxyId:
              strategy.proxyStrategy.unionSelectKey === 'primary'
                ? null
                : strategy.proxyStrategy.unionValueKey.proxyId,
          }

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
