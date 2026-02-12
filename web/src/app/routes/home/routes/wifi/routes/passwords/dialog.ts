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
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiDialogContext,
  TuiError,
  TuiIcon,
  TuiInput,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiChevron, TuiPassword, TuiSelect } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ModalHelp } from 'src/app/directives/modal-help'

import { WifiPasswordsDialogAside } from './dialog-aside'

export interface WifiPasswordEntry {
  label: string
  password: string
  profile: string
}

export interface WifiPasswordDialogData {
  profiles: string[]
  entry?: WifiPasswordEntry
}

export interface WifiPasswordDialogResult {
  label: string
  password?: string
  profile: string
}

@Component({
  template: `
    <wifi-passwords-dialog-aside *modalHelp />
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input
          tuiInput
          placeholder="Name for this Wi-Fi network"
          formControlName="label"
        />
      </tui-textfield>
      <tui-error formControlName="label" />
      @if (!editing) {
        <tui-textfield>
          <label tuiLabel>Password</label>
          <input tuiInput formControlName="password" type="password" />
          <tui-icon tuiPassword />
          <button
            tuiIconButton
            type="button"
            iconStart="@tui.refresh-cw"
            size="s"
            (click)="generatePassword()"
          >
            Generate
          </button>
        </tui-textfield>
        <tui-error formControlName="password" />
      }
      <tui-textfield tuiChevron>
        <label tuiLabel>Security Profile</label>
        <input tuiSelect formControlName="profile" />
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
      <tui-error formControlName="profile" />
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
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiButton,
    TuiInput,
    TuiIcon,
    TuiPassword,
    TuiSelect,
    TuiChevron,
    TuiDataList,
    TuiAppearance,
    RouterLink,
    ModalHelp,
    WifiPasswordsDialogAside,
  ],
})
class AddWifiPassword {
  protected readonly context =
    injectContext<
      TuiDialogContext<WifiPasswordDialogResult, WifiPasswordDialogData>
    >()

  private readonly entry = this.context.data.entry
  protected readonly editing = !!this.entry
  protected readonly profiles = this.context.data.profiles

  protected readonly form = inject(NonNullableFormBuilder).group({
    label: [this.entry?.label ?? '', Validators.required],
    password: [
      this.entry?.password ?? this.generateRandomPassword(),
      [Validators.required, Validators.minLength(8)],
    ],
    profile: [this.entry?.profile ?? '', Validators.required],
  })

  protected generatePassword(): void {
    this.form.controls.password.setValue(this.generateRandomPassword())
  }

  private generateRandomPassword(): string {
    const chars = 'abcdefghijkmnpqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ23456789'
    return Array.from(
      { length: 16 },
      () => chars[Math.floor(Math.random() * chars.length)],
    ).join('')
  }

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      const { label, password, profile } = this.form.getRawValue()
      this.context.completeWith({
        label,
        profile,
        password: this.editing ? undefined : password,
      })
    }
  }
}

export const ADD_WIFI_PASSWORD = new PolymorpheusComponent(AddWifiPassword)
