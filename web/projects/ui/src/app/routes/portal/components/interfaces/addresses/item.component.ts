import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { DisplayAddress } from '../interface.service'
import { AddressActionsComponent } from './actions.component'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'tr[address]',
  template: `
    @if (address(); as address) {
      <td>
        <div class="wrapper">{{ address.type }}</div>
      </td>
      <td>
        <div class="wrapper">
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
        </div>
      </td>
      <td [style.order]="-1">
        <div class="wrapper" [title]="address.gatewayName">
          {{ address.gatewayName || '-' }}
        </div>
      </td>
      <td>
        <div class="wrapper" [title]="address.url">{{ address.url }}</div>
      </td>
      <td
        actions
        [disabled]="!isRunning()"
        [href]="address.url"
        [bullets]="address.bullets"
        [style.width.rem]="5"
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

    :host-context(.uncommon-hidden) {
      .wrapper {
        height: 0;
        visibility: hidden;
      }

      td {
        padding-block: 0;
        border: hidden;
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
  imports: [i18nPipe, AddressActionsComponent, TuiBadge],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  readonly address = input.required<DisplayAddress>()
  readonly isRunning = input.required<boolean>()
}
