import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiInput } from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

@Component({
  imports: [FormsModule, TuiButton, TuiInput, TuiPassword, TuiIcon, i18nPipe],
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2 [id]="context.id">{{ 'Unlock Backup' | i18n }}</h2>
        <p>
          {{
            'Enter the password that was used to encrypt this backup.' | i18n
          }}
        </p>
      </hgroup>
    </header>
    <tui-textfield>
      <label tuiLabel>{{ 'Password' | i18n }}</label>
      <input
        tuiInput
        type="password"
        [(ngModel)]="password"
        (keyup.enter)="unlock()"
      />
      <tui-icon tuiPassword />
    </tui-textfield>
    <footer>
      <button tuiButton appearance="flat" (click)="context.completeWith(null)">
        {{ 'Cancel' | i18n }}
      </button>
      <button tuiButton [disabled]="!password" (click)="unlock()">
        {{ 'Unlock' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    footer {
      display: flex;
      justify-content: flex-end;
      gap: 0.5rem;
      margin-top: 1.5rem;
    }
  `,
})
export class UnlockPasswordDialog {
  protected readonly context = injectContext<TuiDialogContext<string | null>>()

  password = ''

  unlock() {
    if (this.password) {
      this.context.completeWith(this.password)
    }
  }
}

export const UNLOCK_PASSWORD = new PolymorpheusComponent(UnlockPasswordDialog)
