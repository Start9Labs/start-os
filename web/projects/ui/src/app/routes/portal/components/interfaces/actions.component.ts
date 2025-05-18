import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { CopyService, DialogService, i18nPipe } from '@start9labs/shared'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
} from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { InterfaceComponent } from './interface.component'
import { DOCUMENT } from '@angular/common'

@Component({
  standalone: true,
  selector: 'td[actions]',
  template: `
    <div class="desktop">
      <ng-content />
      @if (interface.value().type === 'ui') {
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.external-link"
          [disabled]="disabled()"
          (click)="openUI()"
        >
          {{ 'Open' | i18n }}
        </button>
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
        tuiIconButton
        iconStart="@tui.ellipsis-vertical"
        tuiDropdownOpen
        [tuiDropdown]="dropdown"
      >
        {{ 'Actions' | i18n }}
        <ng-template #dropdown let-close>
          <tui-data-list>
            <tui-opt-group>
              @if (interface.value().type === 'ui') {
                <button
                  tuiOption
                  iconStart="@tui.external-link"
                  [disabled]="disabled()"
                  (click)="openUI()"
                >
                  {{ 'Open' | i18n }}
                </button>
              }
              <button tuiOption iconStart="@tui.qr-code" (click)="showQR()">
                {{ 'Show QR' | i18n }}
              </button>
              <button
                tuiOption
                iconStart="@tui.copy"
                (click)="copyService.copy(href()); close()"
              >
                {{ 'Copy URL' | i18n }}
              </button>
            </tui-opt-group>
            <tui-opt-group><ng-content select="[tuiOption]" /></tui-opt-group>
          </tui-data-list>
        </ng-template>
      </button>
    </div>
  `,
  styles: `
    :host {
      text-align: right;
      grid-area: 1 / 2 / 3 / 3;
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
  imports: [TuiButton, TuiDropdown, TuiDataList, i18nPipe],
  providers: [tuiButtonOptionsProvider({ appearance: 'icon' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceActionsComponent {
  private readonly document = inject(DOCUMENT)

  readonly isMobile = inject(TUI_IS_MOBILE)
  readonly dialog = inject(DialogService)
  readonly copyService = inject(CopyService)
  readonly interface = inject(InterfaceComponent)

  readonly href = input.required<string>()
  readonly disabled = input.required<boolean>()

  showQR() {
    this.dialog
      .openComponent(new PolymorpheusComponent(QRModal), {
        size: 'auto',
        closeable: this.isMobile,
        data: this.href(),
      })
      .subscribe()
  }

  openUI() {
    this.document.defaultView?.open(this.href(), '_blank', 'noreferrer')
  }
}
