import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { LoadingService } from '@start9labs/shared'
import {
  TUI_IS_MOBILE,
  tuiMarkControlAsTouchedAndValidate,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiNumberFormat,
  TuiTextfield,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiInputNumber,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

import { MappedDevice, PortForwardsData } from './utils'

@Component({
  template: `
    <form tuiForm [formGroup]="form">
      <tui-textfield tuiChevron>
        <label tuiLabel>External IP</label>
        @if (mobile) {
          <select
            tuiSelect
            formControlName="externalip"
            [items]="context.data.ips()"
          ></select>
        } @else {
          <input tuiSelect formControlName="externalip" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="context.data.ips()"
          />
        }
      </tui-textfield>
      <tui-error
        formControlName="externalip"
        [error]="[] | tuiFieldError | async"
      />
      <tui-textfield>
        <label tuiLabel>External Port</label>
        <input
          tuiInputNumber
          formControlName="externalport"
          [min]="0"
          [max]="65535"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
        />
      </tui-textfield>
      <tui-error
        formControlName="externalport"
        [error]="[] | tuiFieldError | async"
      />
      <tui-textfield tuiChevron [stringify]="stringify">
        <label tuiLabel>Device</label>
        @if (mobile) {
          <select
            tuiSelect
            formControlName="device"
            [items]="context.data.devices()"
          ></select>
        } @else {
          <input tuiSelect formControlName="device" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="context.data.devices()"
          />
        }
      </tui-textfield>
      <tui-error
        formControlName="device"
        [error]="[] | tuiFieldError | async"
      />
      <tui-textfield>
        <label tuiLabel>Internal Port</label>
        <input
          tuiInputNumber
          formControlName="internalport"
          [min]="0"
          [max]="65535"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
        />
      </tui-textfield>
      <tui-error
        formControlName="internalport"
        [error]="[] | tuiFieldError | async"
      />
      <footer>
        <button tuiButton [disabled]="form.invalid" (click)="onSave()">
          Save
        </button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiChevron,
    TuiDataListWrapper,
    TuiError,
    TuiInputNumber,
    TuiNumberFormat,
    TuiFieldErrorPipe,
    TuiTextfield,
    TuiSelect,
    TuiForm,
  ],
})
export class PortForwardsAdd {
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<void, PortForwardsData>>()

  protected readonly form = inject(NonNullableFormBuilder).group({
    externalip: ['', Validators.required],
    externalport: [null as number | null, Validators.required],
    device: [null as MappedDevice | null, Validators.required],
    internalport: [null as number | null, Validators.required],
  })

  protected readonly stringify = ({ ip, name }: MappedDevice) =>
    ip ? `${name} (${ip})` : ''

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)

      return
    }

    const loader = this.loading.open().subscribe()
    const { externalip, externalport, device, internalport } =
      this.form.getRawValue()

    try {
      await this.api.addForward({
        source: `${externalip}:${externalport}`,
        target: `${device?.ip}:${internalport}`,
      })
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
      this.context.$implicit.complete()
    }
  }
}

export const PORT_FORWARDS_ADD = new PolymorpheusComponent(PortForwardsAdd)
