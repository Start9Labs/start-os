import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiObfuscatePipe } from '@taiga-ui/cdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
import { DisplayAddress } from '../interface.service'
import { AddressActionsComponent } from './actions.component'

@Component({
  selector: 'tr[address]',
  template: `
    @if (address(); as address) {
      <td [style.padding-inline-end]="0">
        <div class="wrapper">
          <button
            tuiIconButton
            appearance="flat-grayscale"
            (click)="viewDetails()"
          >
            {{ 'Address details' | i18n }}
            <tui-icon
              class="info"
              icon="@tui.info"
              background="@tui.info-filled"
            />
          </button>
        </div>
      </td>
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
      <td [style.grid-area]="'1 / 1 / 1 / 3'">
        <div class="wrapper" [title]="address.gatewayName">
          {{ address.gatewayName || '-' }}
        </div>
      </td>
      <td [style.grid-area]="'3 / 1 / 3 / 3'">
        <div
          class="wrapper"
          [title]="address.masked && masked() ? '' : address.url"
        >
          {{ address.url | tuiObfuscate: recipe() }}
        </div>
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
      grid-template-columns: fit-content(10rem) 1fr 2rem 2rem;

      td:last-child {
        padding-inline-start: 0;
      }
    }

    .info {
      background: var(--tui-status-info);

      &::after {
        mask-size: 1.5rem;
      }
    }

    :host-context(.uncommon-hidden) {
      .wrapper {
        height: 0;
        visibility: hidden;
      }

      td,
      & {
        padding-block: 0 !important;
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
        align-content: center;
      }

      td:first-child {
        grid-area: 1 / 3 / 4 / 3;
      }

      td:nth-child(2) {
        font: var(--tui-font-text-m);
        font-weight: bold;
        color: var(--tui-text-primary);
        padding-inline-end: 0.5rem;
      }
    }
  `,
  imports: [
    i18nPipe,
    AddressActionsComponent,
    TuiBadge,
    TuiObfuscatePipe,
    TuiButton,
    TuiIcon,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  private readonly dialogs = inject(DialogService)

  readonly address = input.required<DisplayAddress>()
  readonly isRunning = input.required<boolean>()

  readonly masked = signal(true)
  readonly recipe = computed(() =>
    this.address()?.masked && this.masked() ? 'mask' : 'none',
  )

  viewDetails() {
    this.dialogs
      .openAlert(
        `<ul>${this.address()
          .bullets.map(b => `<li>${b}</li>`)
          .join('')}</ul>` as i18nKey,
        { label: 'About this address' as i18nKey },
      )
      .subscribe()
  }
}
