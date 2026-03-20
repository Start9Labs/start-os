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
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'

@Component({
  template: `
    <header tuiHeader [style.margin-block-start.rem]="-0.5">
      <h2 tuiTitle [id]="context.id">{{ context.data.name }}</h2>
      <aside tuiAccessories>
        <button
          tuiIconButton
          appearance="icon"
          size="s"
          [iconStart]="masked() ? '@tui.eye' : '@tui.eye-off'"
          (click)="masked.set(!masked())"
        >
          Toggle visibility
        </button>
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
      <qr-code
        [value]="context.data.config"
        size="352"
        [class.masked]="masked()"
      />
    } @else {
      <tui-textfield [class.masked]="masked()">
        <textarea
          tuiTextarea
          [min]="19"
          [max]="19"
          [readOnly]="true"
          [value]="context.data.config"
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
  styles: `
    .masked {
      filter: blur(8px);
      pointer-events: none;
      user-select: none;
    }
  `,
  hostDirectives: [ModalHelp],
  providers: [provideHelp('/inbound/client/dialog-config')],
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
  protected readonly masked = signal(true)
  protected readonly context =
    injectContext<TuiDialogContext<void, { name: string; config: string }>>()
  protected readonly href = URL.createObjectURL(
    new Blob([this.context.data.config], { type: 'application/octet-stream' }),
  )
}

export const CLIENT_CONFIG = new PolymorpheusComponent(ClientConfigDialog)
