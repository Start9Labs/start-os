import { Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  imports: [TuiButton, i18nPipe],
  template: `
    <p>{{ 'This drive contains existing StartOS data.' | i18n }}</p>
    <ul>
      <li>
        <strong class="g-positive">{{ 'Preserve' | i18n }}</strong>
        {{ 'to keep your data.' | i18n }}
      </li>
      <li>
        <strong class="g-negative">{{ 'Overwrite' | i18n }}</strong>
        {{ 'to discard' | i18n }}
      </li>
    </ul>
    <footer>
      <button
        tuiButton
        appearance="flat-destructive"
        (click)="context.completeWith(false)"
      >
        {{ 'Overwrite' | i18n }}
      </button>
      <button
        tuiButton
        class="preserve-btn"
        (click)="context.completeWith(true)"
      >
        {{ 'Preserve' | i18n }}
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
