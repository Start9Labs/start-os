import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { ErrorService } from '@start9labs/shared'
import {
  tuiMarkControlAsTouchedAndValidate,
  TuiValueChanges,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCheckbox,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiNumberFormat,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiInputNumber,
  TuiNotificationMiddleService,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiElasticContainer, TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

import { MappedDevice, PortForwardsData } from './utils'

@Component({
  template: `
    <form tuiForm [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input tuiInput formControlName="label" />
      </tui-textfield>
      <tui-error formControlName="label" />
      <tui-textfield tuiChevron>
        <label tuiLabel>External IP</label>
        @if (mobile) {
          <select
            tuiSelect
            formControlName="externalip"
            placeholder="Select IP"
            [items]="context.data.ips()"
          ></select>
        } @else {
          <input tuiSelect formControlName="externalip" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper *tuiDropdown [items]="context.data.ips()" />
        }
      </tui-textfield>
      <tui-error formControlName="externalip" />
      <tui-textfield>
        <label tuiLabel>External Port</label>
        <input
          tuiInputNumber
          formControlName="externalport"
          [min]="0"
          [max]="65535"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
          (tuiValueChanges)="checkShow80()"
        />
      </tui-textfield>
      <tui-error formControlName="externalport" />
      <tui-textfield tuiChevron [stringify]="stringify">
        <label tuiLabel>Device</label>
        @if (mobile) {
          <select
            tuiSelect
            formControlName="device"
            placeholder="Select Device"
            [items]="context.data.devices()"
          ></select>
        } @else {
          <input tuiSelect formControlName="device" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            *tuiDropdown
            [items]="context.data.devices()"
          />
        }
      </tui-textfield>
      <tui-error formControlName="device" />
      <tui-textfield>
        <label tuiLabel>Internal Port</label>
        <input
          tuiInputNumber
          formControlName="internalport"
          [min]="0"
          [max]="65535"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
          (tuiValueChanges)="checkShow80()"
        />
      </tui-textfield>
      <tui-error formControlName="internalport" />
      <tui-elastic-container>
        @if (show80) {
          <label tuiLabel>
            <input tuiCheckbox type="checkbox" formControlName="also80" />
            Also forward port 80 to port 443? This is needed for HTTP to HTTPS
            redirects (recommended)
          </label>
        }
      </tui-elastic-container>
      <footer>
        <button tuiButton [disabled]="form.invalid" (click)="onSave()">
          Save
        </button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiButton,
    TuiChevron,
    TuiDataListWrapper,
    TuiError,
    TuiInputNumber,
    TuiNumberFormat,
    TuiSelect,
    TuiForm,
    TuiCheckbox,
    TuiValueChanges,
    TuiElasticContainer,
    TuiInput,
  ],
})
export class PortForwardsAdd {
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)

  show80 = false

  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<void, PortForwardsData>>()

  protected readonly form = inject(NonNullableFormBuilder).group({
    label: ['', Validators.required],
    externalip: [
      this.context.data.ips().length === 1
        ? (this.context.data.ips().at(0) ?? '')
        : '',
      Validators.required,
    ],
    externalport: [null as number | null, Validators.required],
    device: [null as MappedDevice | null, Validators.required],
    internalport: [null as number | null, Validators.required],
    also80: [true],
  })

  protected readonly stringify = ({ ip, name }: MappedDevice) =>
    ip ? `${name} (${ip})` : ''

  protected checkShow80() {
    const { externalport, internalport } = this.form.getRawValue()
    this.show80 = externalport === 443 && internalport === 443
  }

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)

      return
    }

    const loader = this.loading.open('').subscribe()

    const { label, externalip, externalport, device, internalport, also80 } =
      this.form.getRawValue()

    try {
      await this.api.addForward({
        source: `${externalip}:${externalport}`,
        target: `${device!.ip}:${internalport}`,
        label,
      })

      if (externalport === 443 && internalport === 443 && also80) {
        await this.api.addForward({
          source: `${externalip}:80`,
          target: `${device!.ip}:443`,
          label: `${label} (HTTP redirect)`,
        })
      }
    } catch (e: any) {
      console.error(e)
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.context.$implicit.complete()
    }
  }
}

export const PORT_FORWARDS_ADD = new PolymorpheusComponent(PortForwardsAdd)
