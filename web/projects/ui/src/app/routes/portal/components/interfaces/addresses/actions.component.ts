import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  output,
} from '@angular/core'
import { CopyService, DialogService, i18nPipe } from '@start9labs/shared'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
  TuiTextfield,
} from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { InterfaceComponent } from '../interface.component'

@Component({
  selector: 'td[actions]',
  template: `
    <div class="desktop">
      @if (interface.value()?.type === 'ui') {
        <a
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.external-link"
          target="_blank"
          rel="noopener noreferrer"
          [href]="href()"
        >
          {{ 'Open' | i18n }}
        </a>
      }
      <button
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.qr-code"
        (click)="showQR()"
      >
        {{ 'Show QR' | i18n }}
      </button>
      <button
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.copy"
        (click)="copyService.copy(href())"
      >
        {{ 'Copy URL' | i18n }}
      </button>
    </div>
    <div class="mobile">
      <button
        tuiDropdown
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.ellipsis-vertical"
        [tuiAppearanceState]="open ? 'hover' : null"
        [(tuiDropdownOpen)]="open"
      >
        {{ 'Actions' | i18n }}
        <tui-data-list *tuiTextfieldDropdown="let close">
          <button tuiOption new iconStart="@tui.eye" (click)="onDetails.emit()">
            {{ 'Address details' | i18n }}
          </button>
          @if (interface.value()?.type === 'ui') {
            <a
              tuiOption
              new
              iconStart="@tui.external-link"
              target="_blank"
              rel="noopener noreferrer"
              [href]="href()"
            >
              {{ 'Open' | i18n }}
            </a>
          }
          <button tuiOption new iconStart="@tui.qr-code" (click)="showQR()">
            {{ 'Show QR' | i18n }}
          </button>
          <button
            tuiOption
            new
            iconStart="@tui.copy"
            (click)="copyService.copy(href()); close()"
          >
            {{ 'Copy URL' | i18n }}
          </button>
        </tui-data-list>
      </button>
    </div>
  `,
  styles: `
    :host {
      text-align: right;
      grid-area: 1 / 2 / 4 / 3;
      place-content: center;
      white-space: nowrap;
    }

    .mobile {
      display: none;
    }

    :host-context(tui-root._mobile) {
      .desktop {
        display: none;
      }

      .mobile {
        display: block;
      }
    }
  `,
  imports: [TuiButton, TuiDropdown, TuiDataList, i18nPipe, TuiTextfield],
  providers: [tuiButtonOptionsProvider({ appearance: 'icon' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AddressActionsComponent {
  readonly isMobile = inject(TUI_IS_MOBILE)
  readonly dialog = inject(DialogService)
  readonly copyService = inject(CopyService)
  readonly interface = inject(InterfaceComponent)

  readonly href = input.required<string>()
  readonly disabled = input.required<boolean>()

  readonly onDetails = output<void>()

  open = false

  showQR() {
    this.dialog
      .openComponent(new PolymorpheusComponent(QRModal), {
        size: 'auto',
        closeable: this.isMobile,
        data: this.href(),
      })
      .subscribe()
  }
}
