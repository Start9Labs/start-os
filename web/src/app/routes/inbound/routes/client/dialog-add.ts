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
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'
import { VpnServerPeer } from 'src/app/routes/inbound/service'

export interface ClientDialogData {
  serverAddress: string
  usedIps: string[]
}

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input
          tuiInput
          placeholder="What to call the client device"
          formControlName="name"
        />
      </tui-textfield>
      <tui-error formControlName="name" />
      <tui-textfield>
        <label tuiLabel>LAN IP Address</label>
        <input tuiInput formControlName="ip" />
      </tui-textfield>
      <tui-error formControlName="ip" />
      <tui-textfield>
        <label tuiLabel>Public Key (optional)</label>
        <input tuiInput formControlName="public_key" />
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
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  hostDirectives: [ModalHelp],
  providers: [
    provideHelp('/inbound/client/dialog-add'),
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiButton,
    TuiInput,
  ],
})
class AddClient {
  protected readonly context =
    injectContext<TuiDialogContext<VpnServerPeer, ClientDialogData>>()

  private readonly serverAddress = this.context.data.serverAddress
  private readonly usedIps = this.context.data.usedIps

  protected readonly form = inject(NonNullableFormBuilder).group({
    name: ['', Validators.required],
    ip: [this.nextAvailableIp(), Validators.required],
    public_key: [''],
  })

  private nextAvailableIp(): string {
    const prefix = this.serverAddress.replace(/\.\d+$/, '')
    const startOctet = parseInt(this.serverAddress.split('.').pop()!, 10) + 1
    for (let i = startOctet; i <= 254; i++) {
      const candidate = `${prefix}.${i}`
      if (!this.usedIps.includes(candidate)) return candidate
    }
    return `${prefix}.${startOctet}`
  }

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      const { name, ip, public_key } = this.form.getRawValue()
      this.context.completeWith({
        name,
        ip,
        public_key: public_key || undefined,
      })
    }
  }
}

export const ADD_CLIENT = new PolymorpheusComponent(AddClient)
