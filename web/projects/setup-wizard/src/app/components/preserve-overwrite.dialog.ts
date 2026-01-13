import { Component, inject } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  imports: [TuiButton],
  template: `
    <p>This drive contains existing StartOS data.</p>
    <ul>
      <li>
        <strong class="g-positive">Preserve</strong>
        to keep your data.
      </li>
      <li>
        <strong class="g-negative">Overwrite</strong>
        to discard
      </li>
    </ul>
    <footer>
      <button
        tuiButton
        appearance="flat-destructive"
        (click)="context.completeWith(false)"
      >
        Overwrite
      </button>
      <button
        tuiButton
        class="preserve-btn"
        (click)="context.completeWith(true)"
      >
        Preserve
      </button>
    </footer>
  `,
  styles: `
    p {
      margin: 0 0 0.75rem;
    }

    footer {
      display: flex;
      margin-top: 2rem;
      gap: 0.5rem;
      flex-direction: column-reverse;
    }

    .preserve-btn {
      background: var(--tui-status-positive) !important;
    }
  `,
})
export class PreserveOverwriteDialog {
  protected readonly context = injectContext<TuiDialogContext<boolean>>()
}
