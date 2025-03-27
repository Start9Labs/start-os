import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
} from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { InterfaceComponent } from './interface.component'

@Component({
  standalone: true,
  selector: 'td[actions]',
  template: `
    <div class="desktop">
      <ng-content />
      @if (interface.serviceInterface().type === 'ui') {
        <a
          tuiIconButton
          iconStart="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          [href]="actions()"
        >
          Launch UI
        </a>
      }
      <button tuiIconButton iconStart="@tui.qr-code" (click)="showQR()">
        Show QR
      </button>
      <button
        tuiIconButton
        iconStart="@tui.copy"
        (click)="copyService.copy(actions())"
      >
        Copy URL
      </button>
    </div>
    <div class="mobile">
      <button
        tuiIconButton
        iconStart="@tui.ellipsis-vertical"
        tuiDropdownOpen
        [tuiDropdown]="dropdown"
      >
        Actions
        <ng-template #dropdown let-close>
          <tui-data-list>
            <tui-opt-group>
              @if (interface.serviceInterface().type === 'ui') {
                <a
                  tuiOption
                  iconStart="@tui.external-link"
                  target="_blank"
                  rel="noreferrer"
                  [href]="actions()"
                >
                  Launch UI
                </a>
                <button tuiOption iconStart="@tui.qr-code" (click)="showQR()">
                  Show QR
                </button>
                <button
                  tuiOption
                  iconStart="@tui.copy"
                  (click)="copyService.copy(actions()); close()"
                >
                  Copy URL
                </button>
              }
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
  imports: [TuiButton, TuiDropdown, TuiDataList],
  providers: [tuiButtonOptionsProvider({ appearance: 'icon' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceActionsComponent {
  readonly dialogs = inject(TuiResponsiveDialogService)
  readonly copyService = inject(CopyService)
  readonly interface = inject(InterfaceComponent)

  readonly actions = input.required<string>()

  showQR() {
    this.dialogs
      .open(new PolymorpheusComponent(QRModal), {
        size: 'auto',
        label: 'Interface URL',
        data: this.actions(),
      })
      .subscribe()
  }
}
