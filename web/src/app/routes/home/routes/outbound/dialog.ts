import { Component, inject, signal } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiGroup,
  TuiIcon,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiDialogContext } from '@taiga-ui/experimental'
import {
  TuiBlock,
  TuiChevron,
  TuiDataListWrapper,
  TuiFiles,
  TuiRadio,
  TuiSelect,
  TuiSwitch,
  TuiTooltip,
} from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent, injectContext } from '@taiga-ui/polymorpheus'

@Component({
  template: `
    <form tuiForm="m" [style.margin-top.rem]="1" [formGroup]="form">
      <h3 tuiHeader="body-m">Label</h3>
      <tui-textfield>
        <input
          tuiTextfield
          placeholder="What to call this VPN connection"
          formControlName="label"
        />
      </tui-textfield>
      <h3 tuiHeader="body-m">Connection type</h3>
      <div tuiGroup>
        <label tuiBlock="m">
          <input
            type="radio"
            tuiRadio
            formControlName="type"
            value="WireGuard"
          />
          WireGuard
        </label>
        <label tuiBlock="m">
          <input type="radio" tuiRadio formControlName="type" value="OpenVPN" />
          OpenVPN
        </label>
      </div>
      <h3 tuiHeader="body-m">Configuration setup</h3>
      <label tuiInputFiles class="g-action">
        <input tuiInputFiles formControlName="config" />
        <ng-template>Drag and drop config file here</ng-template>
      </label>
      <h3 tuiHeader="body-m">
        VPN Chaining
        <tui-icon tuiTooltip="Send traffic from this VPN through another VPN" />
        <aside tuiAccessories [style.margin-inline-start]="'auto'">
          <input type="checkbox" tuiSwitch formControlName="chaining" />
        </aside>
      </h3>
      @if (form.value.chaining) {
        <tui-textfield tuiChevron>
          <label tuiLabel>VPN Label</label>
          <input tuiSelect formControlName="vpn" />
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="['Proton', 'Mullvad', 'NordVPN']"
          />
        </tui-textfield>
      }
      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton (click)="save()">Save</button>
      </footer>
    </form>
  `,
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    TuiForm,
    TuiTextfield,
    ReactiveFormsModule,
    TuiGroup,
    TuiBlock,
    TuiRadio,
    TuiFiles,
    TuiHeader,
    TuiSwitch,
    TuiIcon,
    TuiTooltip,
    TuiSelect,
    TuiDataListWrapper,
    TuiChevron,
    TuiButton,
  ],
})
export class AddVPN {
  protected readonly context = injectContext<TuiDialogContext<any>>()
  protected readonly form = inject(NonNullableFormBuilder).group({
    label: ['', Validators.required],
    type: 'WireGuard',
    config: null,
    chaining: false,
    vpn: 'Proton',
  })

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      this.context.completeWith(this.form.value)
    }
  }
}

export const ADD = new PolymorpheusComponent(AddVPN)
