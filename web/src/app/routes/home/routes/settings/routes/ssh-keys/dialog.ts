import { Component, inject, signal } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
  Validators,
} from '@angular/forms'
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
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent, injectContext } from '@taiga-ui/polymorpheus'

function publicKeyValidator(control: AbstractControl): ValidationErrors | null {
  const value = control.value?.trim()
  if (!value) return null

  const parts = value.split(/\s+/)
  if (parts.length < 2) return { invalidKey: true }

  const validAlgorithms =
    /^(ssh-(ed25519|rsa|dss)|ecdsa-sha2-nistp(256|384|521))$/
  if (!validAlgorithms.test(parts[0])) return { invalidKey: true }

  const base64 = /^[A-Za-z0-9+/]+=*$/
  if (!base64.test(parts[1])) return { invalidKey: true }

  return null
}

@Component({
  template: `
    <form tuiForm="m" [style.margin-top.rem]="1" [formGroup]="form">
      <h3 tuiHeader="body-m">Public Key *</h3>
      <tui-textfield>
        <input
          tuiInput
          placeholder="ssh-ed25519 AAAA... user@host"
          formControlName="key"
        />
      </tui-textfield>
      <tui-error formControlName="key" />
      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton (click)="save()">Add Key</button>
      </footer>
    </form>
  `,
  providers: [
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    tuiValidationErrorsProvider({
      invalidKey: 'Invalid public key format',
    }),
  ],
  imports: [
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiInput,
    ReactiveFormsModule,
    TuiHeader,
    TuiButton,
  ],
})
export class AddSshKey {
  protected readonly context = injectContext<TuiDialogContext<string, void>>()
  protected readonly form = inject(NonNullableFormBuilder).group({
    key: ['', [Validators.required, publicKeyValidator]],
  })

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      this.context.completeWith(this.form.value.key!)
    }
  }
}

export const ADD_SSH_KEY = new PolymorpheusComponent(AddSshKey)
