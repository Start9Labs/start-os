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
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { QRComponent } from 'src/app/common/qr.component'

@Component({
  standalone: true,
  selector: 'app-interface',
  template: `
    <div tuiCell>
      <h3 tuiTitle>
        {{ label }}
        <span tuiSubtitle>{{ hostname }}</span>
      </h3>
      <button
        *ngIf="isUi"
        tuiIconButton
        iconLeft="tuiIconExternalLink"
        appearance="icon"
        (click)="launch(hostname)"
      >
        Launch
      </button>
      <button
        tuiIconButton
        iconLeft="tuiIconGrid"
        appearance="icon"
        (click)="showQR(hostname)"
      >
        Show QR code
      </button>
      <button
        tuiIconButton
        iconLeft="tuiIconCopy"
        appearance="icon"
        (click)="copyService.copy(hostname)"
      >
        Copy QR code
      </button>
    </div>
  `,
  imports: [NgIf, TuiCellModule, TuiTitleModule, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceComponent {
  private readonly window = inject(WINDOW)
  private readonly dialogs = inject(TuiDialogService)
  readonly copyService = inject(CopyService)

  @Input({ required: true }) label = ''
  @Input({ required: true }) hostname = ''
  @Input({ required: true }) isUi = false

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
}
