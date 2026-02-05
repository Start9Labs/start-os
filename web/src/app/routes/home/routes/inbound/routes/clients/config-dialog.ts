import { ChangeDetectionStrategy, Component, signal } from '@angular/core'
import {
  TuiButton,
  TuiDialogContext,
  TuiIcon,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCopy, TuiSegmented, TuiTextarea } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QrCodeComponent } from 'ng-qrcode'

@Component({
  template: `
    <header tuiHeader>
      <h2 tuiTitle>Device Config</h2>
      <aside tuiAccessories>
        <tui-segmented [(activeItemIndex)]="index">
          <button>
            <tui-icon icon="@tui.file" />
            File
          </button>
          <button>
            <tui-icon icon="@tui.qr-code" />
            QR
          </button>
        </tui-segmented>
      </aside>
    </header>
    @if (index()) {
      <qr-code [value]="config" size="352" />
    } @else {
      <tui-textfield>
        <textarea
          tuiTextarea
          [min]="16"
          [max]="16"
          [readOnly]="true"
          [value]="config"
        ></textarea>
        <tui-icon tuiCopy />
        <a
          tuiIconButton
          iconStart="@tui.download"
          download="wireguard.conf"
          size="s"
          [href]="href"
        >
          Download
        </a>
      </tui-textfield>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    QrCodeComponent,
    TuiButton,
    TuiHeader,
    TuiIcon,
    TuiTitle,
    TuiSegmented,
    TuiTextfield,
    TuiTextarea,
    TuiCopy,
  ],
})
export class ClientConfigDialog {
  protected readonly index = signal(0)
  protected readonly config =
    injectContext<TuiDialogContext<void, string>>().data
  protected readonly href = URL.createObjectURL(
    new Blob([this.config], { type: 'application/octet-stream' }),
  )
}

export const CLIENT_CONFIG = new PolymorpheusComponent(ClientConfigDialog)
