import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { i18nPipe, i18nKey } from '@start9labs/shared'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiTextfield,
} from '@taiga-ui/core'
import { PublicDomain, PublicDomainService } from './pd.service'
import { toAuthorityName } from 'src/app/utils/acme'

@Component({
  selector: 'tr[publicDomain]',
  template: `
    <td>{{ publicDomain().fqdn }}</td>
    <td>{{ publicDomain().gateway?.ipInfo?.name }}</td>
    <td class="authority">{{ authority() }}</td>
    <td>
      <button
        tuiIconButton
        tuiDropdown
        size="s"
        appearance="flat-grayscale"
        iconStart="@tui.ellipsis-vertical"
        [tuiAppearanceState]="open ? 'hover' : null"
        [(tuiDropdownOpen)]="open"
      >
        {{ 'More' | i18n }}
        <tui-data-list *tuiTextfieldDropdown>
          <tui-opt-group>
            <button
              tuiOption
              new
              iconStart="@tui.eye"
              (click)="
                service.showDns(
                  publicDomain().fqdn,
                  publicDomain().gateway!,
                  dnsMessage()
                )
              "
            >
              {{ 'View DNS' | i18n }}
            </button>
            <button
              tuiOption
              new
              iconStart="@tui.pencil"
              (click)="service.edit(publicDomain())"
            >
              {{ 'Edit' | i18n }}
            </button>
          </tui-opt-group>
          <tui-opt-group>
            <button
              tuiOption
              new
              iconStart="@tui.trash"
              class="g-negative"
              (click)="service.remove(publicDomain().fqdn)"
            >
              {{ 'Delete' | i18n }}
            </button>
          </tui-opt-group>
        </tui-data-list>
      </button>
    </td>
  `,
  styles: `
    :host {
      grid-template-columns: min-content 1fr min-content;
    }

    td:nth-child(2) {
      order: -1;
      grid-column: span 2;
    }

    td:last-child {
      grid-area: 1 / 3 / 3;
      align-self: center;
      text-align: right;
    }

    :host-context(tui-root._mobile) {
      .authority {
        grid-column: span 2;
      }
      tui-badge {
        vertical-align: bottom;
        margin-inline-start: 0.25rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDataList, TuiDropdown, i18nPipe, TuiTextfield],
})
export class PublicDomainsItemComponent {
  protected readonly service = inject(PublicDomainService)

  open = false

  readonly publicDomain = input.required<PublicDomain>()

  readonly authority = computed(() => toAuthorityName(this.publicDomain().acme))
  readonly dnsMessage = computed<i18nKey>(
    () =>
      `Create one of the DNS records below to cause ${this.publicDomain().fqdn} to resolve to ${this.publicDomain().gateway?.ipInfo.wanIp}` as i18nKey,
  )
}
