import { Component, inject } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
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
  TuiIcon,
  TuiInput,
  TuiNumberFormat,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiInputNumber,
  TuiNotificationMiddleService,
  TuiSelect,
  TuiTooltip,
} from '@taiga-ui/kit'
import { TuiElasticContainer, TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

import { MappedDevice, PortForwardsData } from './utils'

// A range counts up from both the external and internal port, so neither side
// may run past the u16 port space. Mirrors the server-side guard in add_forward.
function portRangeOverflow(group: AbstractControl): ValidationErrors | null {
  const ext = group.get('externalport')?.value
  const int = group.get('internalport')?.value
  const count = group.get('count')?.value ?? 1
  if (count <= 1) return null
  const last = count - 1
  return (ext != null && ext + last > 65535) ||
    (int != null && int + last > 65535)
    ? { portRangeOverflow: true }
    : null
}

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input tuiInput formControlName="label" />
      </tui-textfield>
      <tui-error formControlName="label" />
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
      <tui-textfield
        tuiChevron
        [stringify]="stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>Server</label>
        @if (mobile) {
          <select
            tuiSelect
            formControlName="device"
            placeholder="Select Server"
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
      <tui-textfield>
        <label tuiLabel>Number of Ports</label>
        <input
          tuiInputNumber
          formControlName="count"
          [min]="1"
          [max]="65535"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
        />
        <tui-icon [tuiTooltip]="countHint" />
      </tui-textfield>
      <tui-error formControlName="count" />
      @if (form.errors?.['portRangeOverflow']) {
        <tui-error [error]="'Port range runs past the maximum port (65535)'" />
      }
      @if (!isRange) {
        <tui-textfield>
          <label tuiLabel>Hostname (optional)</label>
          <input
            tuiInput
            formControlName="sni"
            placeholder="host.example.com"
          />
          <tui-icon [tuiTooltip]="hostnameHint" />
        </tui-textfield>
      }
      <tui-elastic-container>
        @if (show80 && !isRange) {
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
    TuiTooltip,
    TuiIcon,
    TuiInput,
  ],
})
export class PortForwardsAdd {
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)

  show80 = false

  protected readonly hostnameHint =
    'Only supported for SSL/TLS services — the gateway routes by the TLS SNI, so several hostnames can share one external port. Leave blank for a plain port forward.'

  protected readonly countHint =
    'Forward this many consecutive ports, counting up from the external and internal ports above. Leave at 1 for a single port. SNI hostnames are not supported for ranges.'

  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<void, PortForwardsData>>()

  protected readonly form = inject(NonNullableFormBuilder).group(
    {
      label: ['', Validators.required],
      externalport: [null as number | null, Validators.required],
      device: [null as MappedDevice | null, Validators.required],
      internalport: [null as number | null, Validators.required],
      sni: [''],
      also80: [true],
      count: [
        1,
        [Validators.required, Validators.min(1), Validators.max(65535)],
      ],
    },
    { validators: portRangeOverflow },
  )

  protected get isRange(): boolean {
    return this.form.controls.count.value > 1
  }

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

    const { label, externalport, device, internalport, sni, also80, count } =
      this.form.getRawValue()

    const isRange = count > 1
    // SNI demux is per-port and can't span a range, so ignore any hostname.
    const hostname = isRange ? '' : sni.trim()

    try {
      // One hostname per entry; the external IP is fixed server-side to the
      // target device's WAN.
      await this.api.addForward({
        externalPort: externalport!,
        target: `${device!.ip}:${internalport}`,
        label,
        sni: hostname ? [hostname] : [],
        count,
      })

      if (
        !isRange &&
        !hostname &&
        externalport === 443 &&
        internalport === 443 &&
        also80
      ) {
        await this.api.addForward({
          externalPort: 80,
          target: `${device!.ip}:443`,
          label: `${label} (HTTP redirect)`,
          sni: [],
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
