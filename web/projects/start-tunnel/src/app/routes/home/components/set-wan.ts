import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { TuiButton, TuiDialogContext, TuiTextfield } from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

export interface SetWanData {
  readonly wanIp: string | null
  readonly options: readonly string[]
}

const DEFAULT_LABEL = 'Default (masquerade)'

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form">
      <tui-textfield
        tuiChevron
        [tuiTextfieldCleaner]="false"
        [stringify]="stringify"
      >
        <label tuiLabel>WAN IP</label>
        @if (mobile) {
          <select tuiSelect formControlName="wanIp" [items]="items"></select>
        } @else {
          <input tuiSelect formControlName="wanIp" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper *tuiDropdown [items]="items" />
        }
      </tui-textfield>
      <footer>
        <button tuiButton type="button" (click)="onSave()">Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiButton,
    TuiChevron,
    TuiDataListWrapper,
    TuiForm,
    TuiSelect,
    TuiTextfield,
  ],
})
export class SetWan {
  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<string | null, SetWanData>>()

  private readonly fb = inject(NonNullableFormBuilder)

  protected readonly items: readonly (string | null)[] = [
    null,
    ...this.context.data.options,
  ]

  protected readonly form = this.fb.group({
    wanIp: this.fb.control<string | null>(this.context.data.wanIp),
  })

  protected readonly stringify = (ip: string | null) => ip ?? DEFAULT_LABEL

  protected onSave(): void {
    this.context.completeWith(this.form.getRawValue().wanIp)
  }
}

export const SET_WAN = new PolymorpheusComponent(SetWan)
