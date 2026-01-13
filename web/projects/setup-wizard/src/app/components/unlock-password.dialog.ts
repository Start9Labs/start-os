import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import {
  TuiButton,
  TuiDialogContext,
  TuiIcon,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  imports: [FormsModule, TuiButton, TuiTextfield, TuiPassword, TuiIcon],
  template: `
    <p>Enter the password that was used to encrypt this backup.</p>
    <tui-textfield>
      <label tuiLabel>Password</label>
      <input
        tuiTextfield
        type="password"
        [(ngModel)]="password"
        (keyup.enter)="unlock()"
      />
      <tui-icon tuiPassword />
    </tui-textfield>
    <footer>
      <button tuiButton appearance="flat" (click)="context.completeWith(null)">
        Cancel
      </button>
      <button tuiButton [disabled]="!password" (click)="unlock()">
        Unlock
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
