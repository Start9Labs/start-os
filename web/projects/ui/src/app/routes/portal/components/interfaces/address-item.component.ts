import { TuiCell } from '@taiga-ui/layout'
import { TuiBadge } from '@taiga-ui/kit'
import { NgIf } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { CopyService } from '@start9labs/shared'
import { TuiDialogService, TuiTitle, TuiButton } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { mask } from 'src/app/utils/mask'
import { InterfaceComponent } from './interface.component'
import { AddressesService } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-address-item',
  template: `
    <div tuiCell>
      <tui-badge appearance="success">
        {{ label }}
      </tui-badge>
      <h3 tuiTitle>
        <span tuiSubtitle>
          {{ interface.serviceInterface.masked ? mask : address }}
        </span>
      </h3>
      <button
        *ngIf="interface.serviceInterface.type === 'ui'"
        tuiIconButton
        iconStart="@tui.external-link"
        appearance="icon"
        (click)="launch(address)"
      >
        Launch
      </button>
      <button
        tuiIconButton
        iconStart="@tui.qr-code"
        appearance="icon"
        (click)="showQR(address)"
      >
        Show QR code
      </button>
      <button
        tuiIconButton
        iconStart="@tui.copy"
        appearance="icon"
        (click)="copyService.copy(address)"
      >
        Copy URL
      </button>
      <button
        tuiIconButton
        iconStart="@tui.trash"
        appearance="icon"
        (click)="service.remove()"
      >
        Destroy
      </button>
    </div>
  `,
  imports: [NgIf, TuiCell, TuiTitle, TuiButton, TuiBadge],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AddressItemComponent {
  private readonly window = inject(WINDOW)
  private readonly dialogs = inject(TuiDialogService)

  readonly service = inject(AddressesService)
  readonly copyService = inject(CopyService)
  readonly interface = inject(InterfaceComponent)

  @Input() label?: string
  @Input({ required: true }) address!: string

  get mask(): string {
    return mask(this.address, 64)
  }

  launch(url: string): void {
    this.window.open(url, '_blank', 'noreferrer')
  }

  showQR(data: string) {
    this.dialogs
      .open(new PolymorpheusComponent(QRModal), {
        size: 'auto',
        data,
      })
      .subscribe()
  }
}
