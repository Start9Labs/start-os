import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { i18nPipe, Keyboard, KeyboardLayout } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiInput } from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  template: `
    <tui-textfield
      tuiChevron
      [stringify]="stringify"
      [tuiTextfieldCleaner]="false"
    >
      <label tuiLabel>{{ 'Keyboard' | i18n }}</label>
      @if (mobile) {
        <select tuiSelect [(ngModel)]="selected" [items]="keyboards"></select>
      } @else {
        <input tuiSelect [(ngModel)]="selected" />
      }
      @if (!mobile) {
        <tui-data-list-wrapper *tuiDropdown [items]="keyboards" />
      }
    </tui-textfield>
    <footer>
      <button tuiButton appearance="secondary" (click)="cancel()">
        {{ 'Cancel' | i18n }}
      </button>
      <button
        tuiButton
        [disabled]="!selected || selected.layout === initialLayout"
        (click)="confirm()"
      >
        {{ 'Confirm' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    p {
      margin-bottom: 1rem;
    }

    footer {
      display: flex;
      gap: 1rem;
      margin-top: 1.5rem;
    }
  `,
  imports: [
    FormsModule,
    TuiButton,
    TuiInput,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    i18nPipe,
  ],
})
export class KeyboardSelectComponent {
  private readonly context =
    injectContext<
      TuiDialogContext<
        Keyboard | null,
        { keyboards: Keyboard[]; currentLayout: KeyboardLayout | null }
      >
    >()

  protected readonly mobile = inject(WA_IS_MOBILE)
  readonly keyboards = this.context.data.keyboards
  readonly initialLayout = this.context.data.currentLayout
  selected =
    this.keyboards.find(kb => kb.layout === this.initialLayout) ||
    this.keyboards[0]!

  readonly stringify = (kb: Keyboard) => kb.name

  cancel() {
    this.context.completeWith(null)
  }

  confirm() {
    this.context.completeWith(this.selected)
  }
}
