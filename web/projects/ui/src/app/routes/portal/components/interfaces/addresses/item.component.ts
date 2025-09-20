import {
  ChangeDetectionStrategy,
  Component,
  input,
  inject,
} from '@angular/core'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { DisplayAddress } from '../interface.service'
import { AddressActionsComponent } from './actions.component'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'tr[address]',
  template: `
    @if (address(); as address) {
      <td [style.width.rem]="3">
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.info"
          (click)="viewDetails(address.bullets)"
        >
          {{ 'Address details' | i18n }}
        </button>
      </td>
      <td>{{ address.type }}</td>
      <td>
        @if (address.access === 'public') {
          <tui-badge size="s" appearance="primary-success">
            {{ 'public' | i18n }}
          </tui-badge>
        } @else if (address.access === 'private') {
          <tui-badge size="s" appearance="primary-destructive">
            {{ 'private' | i18n }}
          </tui-badge>
        } @else {
          -
        }
      </td>
      <td [style.order]="-1">
        <div [title]="address.gatewayName">
          {{ address.gatewayName || '-' }}
        </div>
      </td>
      <td>
        <div [title]="address.url">{{ address.url }}</div>
      </td>
      <td
        actions
        [disabled]="!isRunning()"
        [href]="address.url"
        [style.width.rem]="5"
        (onDetails)="viewDetails(address.bullets)"
      ></td>
    }
  `,
  styles: `
    :host {
      white-space: nowrap;

      td:last-child {
        padding-inline-start: 0;
      }
    }

    div {
      white-space: normal;
      word-break: break-all;
      display: -webkit-box;
      -webkit-box-orient: vertical;
      -webkit-line-clamp: 1;
      overflow: hidden;
    }

    :host-context(tui-root._mobile) {
      td {
        width: auto !important;
      }

      td:first-child {
        display: none;
      }

      td:nth-child(2) {
        font: var(--tui-font-text-m);
        font-weight: bold;
        color: var(--tui-text-primary);
      }
    }
  `,
  imports: [i18nPipe, AddressActionsComponent, TuiButton, TuiBadge],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  readonly address = input.required<DisplayAddress>()
  readonly isRunning = input.required<boolean>()
  readonly dialog = inject(DialogService)

  viewDetails(bullets: string[]) {
    this.dialog
      .openAlert(
        `<ul>${bullets.map(b => `<li>${b}</li>`).join('')}</ul>` as i18nKey,
        {
          label: 'About this address' as i18nKey,
        },
      )
      .subscribe()
  }
}
