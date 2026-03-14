import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { i18nPipe, normalizeHostname } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiInput } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  template: `
    <tui-textfield>
      <label tuiLabel>{{ 'Server Name' | i18n }}</label>
      <input tuiInput [(ngModel)]="name" />
    </tui-textfield>
    @if (name.trim()) {
      <p class="hostname-preview">{{ normalizeHostname(name) }}.local</p>
    }
    <footer>
      <button tuiButton appearance="secondary" (click)="cancel()">
        {{ 'Cancel' | i18n }}
      </button>
      <button tuiButton [disabled]="!name.trim()" (click)="confirm()">
        {{ 'Save' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    .hostname-preview {
      color: var(--tui-text-secondary);
      font: var(--tui-typography-body-s);
      margin-top: 0.25rem;
    }

    footer {
      display: flex;
      gap: 1rem;
      margin-top: 1.5rem;
    }
  `,
  imports: [FormsModule, TuiButton, TuiInput, i18nPipe],
})
export class ServerNameDialog {
  private readonly context =
    injectContext<
      TuiDialogContext<
        { name: string; hostname: string } | null,
        { initialName: string }
      >
    >()

  name = this.context.data.initialName
  readonly normalizeHostname = normalizeHostname

  cancel() {
    this.context.completeWith(null)
  }

  confirm() {
    const name = this.name.trim()
    this.context.completeWith({
      name,
      hostname: normalizeHostname(name),
    })
  }
}
