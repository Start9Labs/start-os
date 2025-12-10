import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { Help } from 'src/app/directives/help'

import { LogsAside } from './aside'

@Component({
  template: `
    <logs-aside *help />
    <pre>Initializing...</pre>
    <footer class="g-footer">
      <button tuiButton appearance="outline" iconStart="@tui.download">
        Download
      </button>
      <button tuiButton appearance="outline" iconStart="@tui.arrow-down">
        Scroll to Bottom
      </button>
    </footer>
  `,
  styles: `
    :host {
      height: calc(100% - 6rem);
      display: flex;
      flex-direction: column;
    }

    pre {
      flex: 1;
      padding: 1rem;
      margin: 1rem 0;
      background: var(--tui-background-neutral-1);
      border-radius: var(--tui-radius-s);
    }

    button:first-child {
      margin-inline-end: auto;
    }
  `,
  host: { class: 'g-page' },
  imports: [LogsAside, Help, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Logs {}
