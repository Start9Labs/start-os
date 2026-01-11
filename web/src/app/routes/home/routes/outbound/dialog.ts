import { AsyncPipe } from '@angular/common'
import { Component, inject, signal } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiError,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiDialogContext } from '@taiga-ui/experimental'
import {
  TUI_VALIDATION_ERRORS,
  TuiChevron,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiFile,
  TuiFiles,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent, injectContext } from '@taiga-ui/polymorpheus'
import { getAddOutboundVpnForm, OUTBOUND_VALIDATION_ERRORS } from './utils'

interface AddVPNData {
  targetOptions: string[]
}

@Component({
  template: `
    <form tuiForm="m" [style.margin-top.rem]="1" [formGroup]="form">
      <h3 tuiHeader="body-m">Label *</h3>
      <tui-textfield>
        <input
          tuiTextfield
          placeholder="e.g. Mullvad Sweden"
          formControlName="label"
        />
      </tui-textfield>
      <tui-error formControlName="label" [error]="[] | tuiFieldError | async" />
      <h3 tuiHeader="body-m">WireGuard Configuration *</h3>
      @if (!form.value.config || form.controls.config.invalid) {
        <label tuiInputFiles class="g-action">
          <input tuiInputFiles accept=".conf" formControlName="config" />
          <ng-template>Drop .conf file here or click to browse</ng-template>
        </label>
      } @else {
        <tui-file
          [file]="form.value.config"
          (remove)="form.controls.config.reset()"
        />
      }
      <tui-error
        formControlName="config"
        [error]="[] | tuiFieldError | async"
      />
      <h3 tuiHeader="body-m">Target</h3>
      <tui-textfield tuiChevron>
        <input tuiSelect formControlName="target" />
        <tui-data-list-wrapper
          *tuiTextfieldDropdown
          new
          [items]="context.data.targetOptions"
        />
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
        <button tuiButton [disabled]="form.invalid" (click)="save()">
          Add VPN
        </button>
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
    { provide: TUI_VALIDATION_ERRORS, useValue: OUTBOUND_VALIDATION_ERRORS },
  ],
  imports: [
    AsyncPipe,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
    ReactiveFormsModule,
    TuiFiles,
    TuiFile,
    TuiHeader,
    TuiSelect,
    TuiDataListWrapper,
    TuiChevron,
    TuiButton,
  ],
})
export class AddVPN {
  protected readonly context =
    injectContext<TuiDialogContext<any, AddVPNData>>()
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

export const ADD = new PolymorpheusComponent(AddVPN)
