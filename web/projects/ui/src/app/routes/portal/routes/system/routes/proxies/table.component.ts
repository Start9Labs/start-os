import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogOptions,
  TuiDialogService,
  TuiLink,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import {
  DELETE_OPTIONS,
  ProxyUpdate,
} from 'src/app/routes/portal/routes/system/routes/proxies/constants'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { Proxy } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'table[proxies]',
  template: `
    <thead>
      <tr>
        <th>Name</th>
        <th>Type</th>
        <th>Used By</th>
        <th [style.width.rem]="3.5"></th>
      </tr>
    </thead>
    <tbody>
      @for (proxy of proxies; track $index) {
        <tr>
          <td class="title">{{ proxy.name }}</td>
          <td class="type">{{ proxy.type }}</td>
          <td class="used">
            @if (getLength(proxy); as length) {
              <button tuiLink (click)="onUsedBy(proxy)">
                Used by: {{ length }}
              </button>
            } @else {
              N/A
            }
          </td>
          <td class="actions">
            <button
              tuiIconButton
              appearance="icon"
              size="xs"
              iconStart="@tui.pencil"
              (click)="rename(proxy)"
            >
              Rename
            </button>
            <button
              tuiIconButton
              appearance="icon"
              size="xs"
              iconStart="@tui.trash-2"
              (click)="delete(proxy)"
            >
              Delete
            </button>
          </td>
        </tr>
      } @empty {
        @if (proxies) {
          <tr><td colspan="5">No proxies added</td></tr>
        } @else {
          <tr>
            <td colspan="5"><div [tuiSkeleton]="true">Loading</div></td>
          </tr>
        }
      }
    </tbody>
  `,
  styles: `
    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr 1fr;
      }

      td:only-child {
        grid-column: span 2;
      }

      .title {
        order: 1;
        font-weight: bold;
        text-transform: uppercase;
      }

      .actions {
        order: 2;
        padding: 0;
        text-align: right;
      }

      .type {
        order: 3;
      }

      .used {
        order: 4;
        text-align: right;

        &:not(:has(button)) {
          display: none;
        }
      }
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiLink, TuiButton, TuiSkeleton],
})
export class ProxiesTableComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)

  @Input()
  proxies: readonly Proxy[] | null = null

  getLength({ usedBy }: Proxy) {
    return usedBy.domains.length + usedBy.services.length
  }

  onUsedBy({ name, usedBy }: Proxy) {
    let message = `Proxy "${name}" is currently used by:`
    const domains = usedBy.domains.map(d => `<li>${d}</li>`)
    const services = usedBy.services.map(s => `<li>${s.title}</li>`)

    if (usedBy.domains.length) {
      message = `${message}<h2>Domains (inbound)</h2><ul>${domains}</ul>`
    }

    if (usedBy.services.length) {
      message = `${message}<h2>Services (outbound)</h2>${services}`
    }

    this.dialogs.open(message, { label: 'Used by', size: 's' }).subscribe()
  }

  delete({ id }: Proxy) {
    this.dialogs
      .open(TUI_CONFIRM, DELETE_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()

        try {
          await this.api.deleteProxy({ id })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async rename(proxy: Proxy) {
    const spec = { name: 'Name', required: true, default: proxy.name }
    const name = await ISB.Value.text(spec).build({} as any)
    const options: Partial<TuiDialogOptions<FormContext<{ name: string }>>> = {
      label: `Rename ${proxy.name}`,
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

    this.formDialog.open(FormComponent, options)
  }

  private async update(value: ProxyUpdate): Promise<boolean> {
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
}
