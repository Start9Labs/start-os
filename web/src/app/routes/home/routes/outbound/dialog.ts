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
import { ModalHelp } from 'src/app/directives/modal-help'

import { OutboundDialogAside } from './aside'
import { getAddOutboundVpnForm, OUTBOUND_VALIDATION_ERRORS } from './utils'

@Component({
  template: `
    <outbound-dialog-aside *modalHelp />
    <form tuiForm="m" [style.margin-top.rem]="1" [formGroup]="form">
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
        <tui-data-list-wrapper *tuiDropdown [items]="context.data" />
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
        <button tuiButton (click)="save()">Add VPN</button>
      </footer>
    </form>
  `,
  styles: `
    [tuiInputFiles] {
      min-height: 8rem;
    }
  `,
  providers: [
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
    ModalHelp,
    OutboundDialogAside,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
class AddClient {
  protected readonly context = injectContext<TuiDialogContext<any, string[]>>()
  protected readonly form = getAddOutboundVpnForm(
    inject(NonNullableFormBuilder),
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
