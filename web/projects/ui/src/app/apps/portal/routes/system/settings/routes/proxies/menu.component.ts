import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiDataListModule,
  TuiDialogOptions,
  TuiDialogService,
  TuiDropdownModule,
  TuiHostedDropdownModule,
} from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { Proxy } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormContext, FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DELETE_OPTIONS, ProxyUpdate } from './constants'

@Component({
  selector: 'proxies-menu',
  template: `
    <tui-hosted-dropdown
      style="float: right"
      tuiDropdownAlign="left"
      [sided]="true"
      [content]="dropdown"
    >
      <button
        tuiIconButton
        type="button"
        appearance="icon"
        size="s"
        iconLeft="tuiIconMoreHorizontal"
      ></button>
    </tui-hosted-dropdown>
    <ng-template #dropdown>
      <tui-data-list>
        <button
          *ngIf="!proxy.primaryInbound && proxy.type === 'inbound-outbound'"
          tuiOption
          (click)="update({ primaryInbound: true })"
        >
          Make Primary Inbound
        </button>
        <button
          *ngIf="
            !proxy.primaryOutbound &&
            (proxy.type === 'inbound-outbound' || proxy.type === 'outbound')
          "
          tuiOption
          (click)="update({ primaryOutbound: true })"
        >
          Make Primary Outbound
        </button>
        <button tuiOption (click)="rename()">Rename</button>
        <tui-opt-group>
          <button tuiOption (click)="delete()">Delete</button>
        </tui-opt-group>
      </tui-data-list>
    </ng-template>
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButtonModule,
    TuiDataListModule,
    TuiDropdownModule,
    TuiHostedDropdownModule,
  ],
})
export class ProxiesMenuComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)

  @Input({ required: true }) proxy!: Proxy

  delete() {
    this.dialogs
      .open(TUI_PROMPT, DELETE_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()

        try {
          await this.api.deleteProxy({ id: this.proxy.id })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async update(value: ProxyUpdate): Promise<boolean> {
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

  async rename() {
    const spec = { name: 'Name', required: { default: this.proxy.name } }
    const name = await Value.text(spec).build({} as any)
    const options: Partial<TuiDialogOptions<FormContext<{ name: string }>>> = {
      label: `Rename ${this.proxy.name}`,
      data: {
        spec: { name },
        buttons: [
          {
            text: 'Save',
            handler: value => this.update(value),
          },
        ],
      },
    }

    this.formDialog.open(FormPage, options)
  }
}
