import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton, TuiDialogContext, TuiIcon } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  selector: 'ip-changed-dialog',
  template: `
    <p>
      Your router's IP address has changed. The UI is now available at the new
      address.
    </p>
    <footer>
      <a tuiButton [href]="'http://' + context.data" target="_blank">
        <tui-icon icon="@tui.external-link" />
        Open
      </a>
    </footer>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    footer {
      display: flex;
      justify-content: flex-end;
    }

    tui-icon {
      font-size: 1rem;
    }
  `,
  imports: [TuiButton, TuiIcon],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IpChangedDialog {
  protected readonly context = injectContext<TuiDialogContext<void, string>>()
}
