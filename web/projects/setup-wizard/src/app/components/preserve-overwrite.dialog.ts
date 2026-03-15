import { Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

@Component({
  imports: [TuiButton, TuiHeader, TuiTitle, i18nPipe],
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2 [id]="context.id">{{ 'StartOS Data Detected' | i18n }}</h2>
        <p>{{ 'This drive contains existing StartOS data.' | i18n }}</p>
      </hgroup>
    </header>
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
        appearance=""
        [style.background]="'var(--tui-status-positive)'"
        (click)="context.completeWith(true)"
      >
        {{ 'Preserve' | i18n }}
      </button>
    </footer>
  `,
})
export class PreserveOverwriteDialog {
  protected readonly context = injectContext<TuiDialogContext<boolean>>()
}

export const PRESERVE_OVERWRITE = new PolymorpheusComponent(
  PreserveOverwriteDialog,
)
