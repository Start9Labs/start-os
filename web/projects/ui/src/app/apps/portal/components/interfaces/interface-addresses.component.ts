import { NgIf } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { CopyService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import {
  TuiBadgeModule,
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { QRComponent } from 'src/app/common/qr.component'
import { mask } from 'src/app/util/mask'

@Component({
  standalone: true,
  selector: 'app-interface-address',
  template: `
    <div tuiCell>
      <tui-badge appearance="success">
        {{ label }}
      </tui-badge>
      <h3 tuiTitle>
        <span tuiSubtitle>{{ isMasked ? mask : address }}</span>
      </h3>
      <button
        *ngIf="isUi"
        tuiIconButton
        iconLeft="tuiIconExternalLink"
        appearance="icon"
        (click)="launch(address)"
      >
        Launch
      </button>
      <button
        tuiIconButton
        iconLeft="tuiIconGrid"
        appearance="icon"
        (click)="showQR(address)"
      >
        Show QR code
      </button>
      <button
        tuiIconButton
        iconLeft="tuiIconCopy"
        appearance="icon"
        (click)="copyService.copy(address)"
      >
        Copy URL
      </button>
      <button
        tuiIconButton
        iconLeft="tuiIconTrash"
        appearance="icon"
        (click)="destroy()"
      >
        Destroy
      </button>
    </div>
  `,
  imports: [
    NgIf,
    TuiCellModule,
    TuiTitleModule,
    TuiButtonModule,
    TuiBadgeModule,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressComponent {
  private readonly window = inject(WINDOW)
  private readonly dialogs = inject(TuiDialogService)
  readonly copyService = inject(CopyService)

  @Input() label?: string
  @Input({ required: true }) address!: string
  @Input({ required: true }) isMasked!: boolean
  @Input({ required: true }) isUi!: boolean

  get mask(): string {
    return mask(this.address, 64)
  }

  launch(url: string): void {
    this.window.open(url, '_blank', 'noreferrer')
  }

  showQR(data: string) {
    this.dialogs
      .open(new PolymorpheusComponent(QRComponent), {
        size: 'auto',
        data,
      })
      .subscribe()
  }

  destroy() {}
}
