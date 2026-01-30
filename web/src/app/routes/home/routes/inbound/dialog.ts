import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { RouterLink } from '@angular/router'
import { TuiAutoFocus, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiNumberFormat,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiChevron, TuiInputNumber, TuiSelect } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ModalHelp } from 'src/app/directives/modal-help'
import { Inbound } from 'src/app/routes/home/routes/inbound/service'

import { InboundDialogAside } from './aside'

@Component({
  template: `
    <inbound-dialog-aside *modalHelp />
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input
          tuiInput
          tuiAutoFocus
          placeholder="What to call this VPN connection"
          formControlName="label"
        />
      </tui-textfield>
      <tui-error formControlName="label" />
      <tui-textfield tuiChevron>
        <label tuiLabel>Address</label>
        <input tuiSelect formControlName="address" />
        <tui-data-list *tuiDropdown>
          @for (item of keys; track $index) {
            <button tuiOption [value]="item">
              {{ item }}
              @if (addresses[item]) {
                <span class="g-secondary">({{ addresses[item] }})</span>
              }
            </button>
          }
        </tui-data-list>
      </tui-textfield>
      <tui-error formControlName="address" />
      <tui-textfield tuiChevron>
        <label tuiLabel>Security Profile</label>
        <input tuiSelect formControlName="securityProfile" />
        <tui-data-list *tuiDropdown>
          <tui-opt-group>
            @for (item of profiles; track $index) {
              <button tuiOption [value]="item">{{ item }}</button>
            }
          </tui-opt-group>
          <tui-opt-group>
            <a
              tuiOption
              tuiAppearance="action"
              routerLink="profiles"
              iconStart="@tui.user-lock"
              (click)="context.$implicit.complete()"
            >
              Manage profiles
            </a>
          </tui-opt-group>
        </tui-data-list>
      </tui-textfield>
      <tui-error formControlName="securityProfile" />
      <tui-textfield
        [tuiNumberFormat]="{ precision: 0, thousandSeparator: '' }"
      >
        <label tuiLabel>Port</label>
        <input tuiInputNumber formControlName="port" />
      </tui-textfield>
      <tui-error formControlName="port" />
      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>{{ context.data ? 'Save VPN' : 'Add VPN' }}</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiButton,
    TuiInput,
    ModalHelp,
    InboundDialogAside,
    TuiSelect,
    TuiChevron,
    TuiDataList,
    RouterLink,
    TuiAppearance,
    TuiInputNumber,
    TuiNumberFormat,
    TuiAutoFocus,
  ],
})
class AddServer {
  protected readonly context =
    injectContext<TuiDialogContext<Inbound, Inbound | undefined>>()

  protected readonly form = inject(NonNullableFormBuilder).group({
    label: [this.context.data?.label ?? '', Validators.required],
    address: [this.context.data?.address ?? 'Custom', Validators.required],
    port: [this.context.data?.port ?? 51820, Validators.required],
    securityProfile: [
      this.context.data?.securityProfile ?? '',
      Validators.required,
    ],
  })

  protected readonly addresses: Record<string, string> = {
    '100.65.227.234': 'WAN IPv4',
    '2001:db8:abcd:1234::2': 'WAN IPv6',
    'agf5d.start9.me': 'DDNS',
    Custom: '',
  }

  protected readonly keys = Object.keys(this.addresses)
  protected readonly profiles = ['Admin', 'Guest']

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      this.context.completeWith({
        ...this.form.getRawValue(),
        id: this.context.data?.id || String(Date.now()),
        enabled: !!this.context.data?.enabled,
        clients: this.context.data?.clients || [],
      })
    }
  }
}

export const ADD_SERVER = new PolymorpheusComponent(AddServer)
