import { DatePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, signal } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiInput, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'

@Component({
  selector: 'security-ssh',
  template: `
    <form tuiForm="m" tuiCardLarge class="g-form" (ngSubmit)="toggle(false)">
      <header tuiHeader>
        <h2 tuiTitle>SSH</h2>
        <aside tuiAccessories>
          <button
            tuiButton
            type="button"
            iconStart="@tui.plus"
            (click)="toggle(true)"
          >
            Add SSH key
          </button>
        </aside>
      </header>
      <table tuiTable class="g-table">
        <thead>
          <tr>
            <th tuiTh>Date</th>
            <th tuiTh>Public Key</th>
            <th tuiTh></th>
          </tr>
        </thead>
        <tbody>
          @for (item of keys(); track $index) {
            <tr>
              <td tuiTd>{{ item.date | date: 'medium' }}</td>
              <td tuiTd>{{ item.key }}</td>
              <td tuiTd>
                <button
                  tuiIconButton
                  type="button"
                  size="xs"
                  appearance="icon"
                  iconStart="@tui.trash"
                  (click)="remove($index)"
                >
                  Delete
                </button>
              </td>
            </tr>
          }
          @if (add()) {
            <tr>
              <td tuiTd></td>
              <td tuiTd>
                <tui-textfield>
                  <input
                    tuiAutoFocus
                    tuiInput
                    name="key"
                    placeholder="ssh-rsa..."
                    [(ngModel)]="value"
                  />
                  <button
                    tuiIconButton
                    iconStart="@tui.check"
                    appearance="positive"
                    [style.border-radius.%]="100"
                  >
                    Save
                  </button>
                </tui-textfield>
              </td>
              <td tuiTd>
                <button
                  tuiIconButton
                  type="button"
                  size="xs"
                  appearance="icon"
                  iconStart="@tui.trash"
                  (click)="value.set(''); toggle(false)"
                >
                  Delete
                </button>
              </td>
            </tr>
          }
        </tbody>
      </table>
    </form>
  `,
  styles: `
    [tuiTd]:last-child {
      padding-block: 0;
    }
  `,
  imports: [
    DatePipe,
    FormsModule,
    TuiButton,
    TuiCardLarge,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiTable,
    TuiTextfield,
    TuiAutoFocus,
    TuiInput,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecuritySSH {
  protected readonly value = signal('')
  protected readonly add = signal(false)
  protected readonly keys = signal([
    {
      date: new Date(),
      key: 'ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEArD...',
    },
    {
      date: new Date(),
      key: 'ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEArD...',
    },
  ])

  protected remove(index: number) {
    this.keys.update(keys => keys.filter((_, i) => i !== index))
  }

  protected toggle(value: boolean) {
    if (value) {
      this.add.set(true)
    } else {
      this.add.set(false)

      if (this.value()) {
        this.keys.update(keys => [
          ...keys,
          { date: new Date(), key: this.value() },
        ])
      }

      this.value.set('')
    }
  }
}
