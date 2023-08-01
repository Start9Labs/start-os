import { Component, ViewChild } from '@angular/core'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiHostedDropdownComponent,
} from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { DataModel, Proxy } from 'src/app/services/patch-db/data-model'
import { FormContext, FormPage } from '../../../modals/form/form.page'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'

@Component({
  selector: 'proxies',
  templateUrl: './proxies.page.html',
  styleUrls: ['./proxies.page.scss'],
})
export class ProxiesPage {
  @ViewChild(TuiHostedDropdownComponent)
  menuComponent?: TuiHostedDropdownComponent

  menuOpen = false

  readonly docsUrl = 'https://docs.start9.com/latest/user-manual/vpns/'

  readonly proxies$ = this.patch.watch$('server-info', 'network', 'proxies')

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async presentModalAdd() {
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
    this.formDialog.open(FormPage, options)
  }

  async presentModalRename(proxy: Proxy) {
    const options: Partial<TuiDialogOptions<FormContext<{ name: string }>>> = {
      label: `Rename ${proxy.name}`,
      data: {
        spec: {
          name: await Value.text({
            name: 'Name',
            required: { default: proxy.name },
          }).build({} as any),
        },
        buttons: [
          {
            text: 'Save',
            handler: value => this.update(value).then(() => true),
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  presentAlertDelete(id: string) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Delete proxy? This action cannot be undone.',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.delete(id)
      })
  }

  presentAlertUsedBy(name: string, usedBy: Proxy['usedBy']) {
    let message = `Proxy "${name}" is currently used by:`
    if (usedBy.domains.length) {
      message = `${message}<h2>Domains (inbound)</h2><ul>${usedBy.domains.map(
        d => `<li>${d}</li>`,
      )}</ul>`
    }
    if (usedBy.services.length) {
      message = `${message}<h2>Services (outbound)</h2>${usedBy.services.map(
        s => `<li>${s.title}</li>`,
      )}`
    }

    this.dialogs
      .open(message, {
        label: 'Used by',
        size: 's',
      })
      .subscribe()
  }

  private async save(value: WireguardSpec): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addProxy({
        name: value.name,
        config: value.config || '',
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async update(
    value: Partial<{
      name: string
      primaryInbound: true
      primaryOutbound: true
    }>,
  ): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.updateProxy(value)
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async delete(id: string): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteProxy({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

const wireguardSpec = Config.of({
  name: Value.text({
    name: 'Name',
    description: 'A friendly name to help you remember and identify this proxy',
    required: { default: null },
  }),
  config: Value.file({
    name: 'Wiregaurd Config',
    required: true,
    extensions: ['.conf'],
  }),
})

type WireguardSpec = typeof wireguardSpec.validator._TYPE
