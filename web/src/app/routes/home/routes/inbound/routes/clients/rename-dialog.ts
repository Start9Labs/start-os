import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import {
  TuiButton,
  TuiDialogContext,
  TuiInput,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input tuiInput formControlName="name" />
      </tui-textfield>
      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ReactiveFormsModule, TuiForm, TuiTextfield, TuiInput, TuiButton],
})
class RenameClient {
  protected readonly context = injectContext<TuiDialogContext<string, string>>()

  protected readonly form = inject(NonNullableFormBuilder).group({
    name: [this.context.data, Validators.required],
  })

  protected save(): void {
    if (this.form.valid) {
      this.context.completeWith(this.form.value.name!)
    }
  }
}

export const RENAME_CLIENT = new PolymorpheusComponent(RenameClient)
