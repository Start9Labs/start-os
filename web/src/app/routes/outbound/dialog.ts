import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiFile,
  TuiFiles,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'
import {
  AddClientDialogData,
  getAddOutboundVpnForm,
  OUTBOUND_VALIDATION_ERRORS,
} from './utils'

@Component({
  template: `
    <form
      tuiForm="m"
      [style.margin-top.rem]="1"
      [formGroup]="form"
      (submit.prevent)="save()"
    >
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input
          tuiInput
          placeholder="e.g. Mullvad Sweden"
          formControlName="label"
        />
      </tui-textfield>
      <tui-error formControlName="label" />
      @if (!form.value.config || form.controls.config.invalid) {
        <label tuiInputFiles class="g-action">
          <input tuiInputFiles accept=".conf" formControlName="config" />
          <ng-template>
            <div>
              Drop WireGuard
              <b>.conf</b>
              file here
              <div>or click to browse</div>
            </div>
          </ng-template>
        </label>
      } @else {
        <tui-file
          [file]="form.value.config"
          (remove)="form.controls.config.reset()"
        />
      }
      <tui-error formControlName="config" />
      <tui-textfield tuiChevron>
        <label tuiLabel>Target</label>
        <input tuiSelect formControlName="target" />
        <tui-data-list-wrapper *tuiDropdown [items]="context.data.targets" />
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
        <button tuiButton>Add VPN</button>
      </footer>
    </form>
  `,
  styles: `
    [tuiInputFiles] {
      min-height: 8rem;
    }
  `,
  hostDirectives: [ModalHelp],
  providers: [
    provideHelp('/outbound/dialog'),
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    tuiValidationErrorsProvider(OUTBOUND_VALIDATION_ERRORS),
  ],
  imports: [
    TuiForm,
    TuiTextfield,
    TuiError,
    ReactiveFormsModule,
    TuiFiles,
    TuiFile,
    TuiSelect,
    TuiDataListWrapper,
    TuiChevron,
    TuiButton,
    TuiInput,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
class AddClient {
  protected readonly context =
    injectContext<TuiDialogContext<any, AddClientDialogData>>()
  protected readonly form = getAddOutboundVpnForm(
    inject(NonNullableFormBuilder),
    this.context.data.existingLabels,
  )

  constructor() {
    this.form.controls.config.valueChanges.subscribe(() => {
      if (this.form.controls.config.invalid) {
        this.form.controls.config.markAsTouched()
      }
    })
  }

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      this.context.completeWith(this.form.value)
    }
  }
}

export const ADD_CLIENT = new PolymorpheusComponent(AddClient)
