import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiObfuscatePipe } from '@taiga-ui/cdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { FormsModule } from '@angular/forms'
import { TuiSwitch } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GatewayAddress, MappedServiceInterface } from '../interface.service'
import { AddressActionsComponent } from './actions.component'

@Component({
  selector: 'tr[address]',
  host: {
    '[class._disabled]': '!address().enabled',
  },
  template: `
    @if (address(); as address) {
      <td>
        <input
          type="checkbox"
          tuiSwitch
          size="s"
          [showIcons]="false"
          [disabled]="toggling()"
          [ngModel]="address.enabled"
          (ngModelChange)="onToggleEnabled()"
        />
      </td>
      <td>
        {{ address.type }}
      </td>
      <td class="access">
        <tui-icon
          [icon]="address.access === 'public' ? '@tui.globe' : '@tui.house'"
        />
        {{ address.access | i18n }}
      </td>
      <td>
        {{ address.certificate }}
      </td>
      <td>
        <div class="url">
          <span
            [title]="address.masked && currentlyMasked() ? '' : address.url"
          >
            {{ address.url | tuiObfuscate: recipe() }}
          </span>
          @if (address.masked) {
            <button
              tuiIconButton
              appearance="flat-grayscale"
              size="xs"
              [iconStart]="currentlyMasked() ? '@tui.eye' : '@tui.eye-off'"
              (click)="currentlyMasked.set(!currentlyMasked())"
            >
              {{ (currentlyMasked() ? 'Reveal' : 'Hide') | i18n }}
            </button>
          }
        </div>
      </td>
      <td
        actions
        [address]="address"
        [packageId]="packageId()"
        [value]="value()"
        [disabled]="!isRunning()"
        [style.width.rem]="5"
      ></td>
    }
  `,
  styles: `
    :host {
      grid-template-columns: fit-content(10rem) 1fr 2rem 2rem;
    }

    .access tui-icon {
      font-size: 1rem;
      vertical-align: middle;
    }

    .url {
      display: flex;
      align-items: center;
      gap: 0.25rem;

      span {
        white-space: normal;
        word-break: break-all;
        display: -webkit-box;
        -webkit-box-orient: vertical;
        -webkit-line-clamp: 1;
        overflow: hidden;
      }
    }

    :host-context(tui-root._mobile) {
      padding-inline-start: 0.75rem !important;

      &::before {
        content: '';
        position: absolute;
        inset-inline-start: 0;
        top: 0.25rem;
        bottom: 0.25rem;
        width: 4px;
        background: var(--tui-status-positive);
        border-radius: 2px;
      }

      &._disabled::before {
        background: var(--tui-background-neutral-1-hover);
      }

      td {
        width: auto !important;
        align-content: center;
      }

      td:first-child {
        display: none;
      }

      td:nth-child(2) {
        font: var(--tui-font-text-m);
        font-weight: bold;
        color: var(--tui-text-primary);
        padding-inline-end: 0.5rem;
      }

      td:nth-child(4) {
        grid-area: 2 / 1 / 2 / 3;
      }

      td:nth-child(5) {
        grid-area: 3 / 1 / 3 / 3;
      }

      td:last-child {
        grid-area: 1 / 3 / 4 / 5;
        align-self: center;
        justify-self: end;
      }
    }
  `,
  imports: [
    i18nPipe,
    AddressActionsComponent,
    TuiButton,
    TuiIcon,
    TuiObfuscatePipe,
    TuiSwitch,
    FormsModule,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)

  readonly address = input.required<GatewayAddress>()
  readonly packageId = input('')
  readonly value = input<MappedServiceInterface | undefined>()
  readonly isRunning = input.required<boolean>()

  readonly toggling = signal(false)
  readonly currentlyMasked = signal(true)
  readonly recipe = computed(() =>
    this.address()?.masked && this.currentlyMasked() ? 'mask' : 'none',
  )

  async onToggleEnabled() {
    const addr = this.address()
    const iface = this.value()
    if (!iface) return

    this.toggling.set(true)
    const enabled = !addr.enabled
    const addressJson = JSON.stringify(addr.hostnameInfo)
    const loader = this.loader.open('Saving').subscribe()

    try {
      if (this.packageId()) {
        await this.api.pkgBindingSetAddressEnabled({
          internalPort: iface.addressInfo.internalPort,
          address: addressJson,
          enabled,
          package: this.packageId(),
          host: iface.addressInfo.hostId,
        })
      } else {
        await this.api.serverBindingSetAddressEnabled({
          internalPort: 80,
          address: addressJson,
          enabled,
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.toggling.set(false)
    }
  }
}
