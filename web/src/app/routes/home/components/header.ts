import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTextfield, tuiTextfieldOptionsProvider } from '@taiga-ui/core'
import { TuiBlock, TuiSwitch } from '@taiga-ui/kit'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'header',
  template: `
    <button (click.stop)="sidebars.start.set(!sidebars.start())">
      <img alt="Start9" src="assets/favicon.svg" />
    </button>
    <tui-textfield iconStart="@tui.search">
      <input tuiTextfield [(ngModel)]="search" />
    </tui-textfield>
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="sidebars.end" />
      Help
    </label>
  `,
  styles: `
    :host {
      grid-column: span 3;
      display: flex;
      align-items: center;
      gap: 0.75rem;
      padding: 0 0.75rem;
      background: var(--tui-background-neutral-2);
      border-bottom: 1px solid var(--tui-border-normal);
    }

    button {
      width: 2rem;
      height: 2rem;
      background: none;
      border: none;
      padding: 0;
      margin-inline-end: auto;
      cursor: pointer;
      pointer-events: none;
    }

    tui-textfield {
      width: min(15rem, calc(100vw - 13rem));
    }

    [tuiTextfield],
    [tuiBlock] {
      border-radius: 2rem;
    }

    :host-context(tui-root._mobile) {
      grid-column: span 1;

      button {
        pointer-events: auto;
      }
    }

    :host-context(body:not([tuiTheme])) {
      button {
        filter: invert(1);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTextfield, TuiBlock, TuiSwitch, FormsModule],
  providers: [
    tuiTextfieldOptionsProvider({
      size: signal('s'),
      appearance: signal('secondary-grayscale'),
    }),
  ],
})
export class Header {
  protected readonly sidebars = inject(SidebarService)
  protected readonly search = signal('')
}
