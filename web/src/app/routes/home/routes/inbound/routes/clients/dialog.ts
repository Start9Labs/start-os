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
  TuiLink,
  TuiNotification,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiSegmented } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QrCodeComponent } from 'ng-qrcode'
import { ModalHelp } from 'src/app/directives/modal-help'
import { Client } from 'src/app/routes/home/routes/inbound/service'

import { InboundClientsDialogAside } from './dialog-aside'

@Component({
  template: `
    <inbound-client-dialog-aside *modalHelp />
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input
          tuiInput
          placeholder="What to call the client device"
          formControlName="name"
        />
      </tui-textfield>
      <tui-error formControlName="name" />
      <tui-textfield>
        <label tuiLabel>LAN IP Address</label>
        <input tuiInput formControlName="address" />
      </tui-textfield>
      <tui-error formControlName="address" />
      <tui-error
        class="g-secondary"
        error="Address must not be used by another client device"
      />
      <tui-segmented [(activeItemIndex)]="index">
        <button type="button">Generate client keys</button>
        <button type="button">Provide existing</button>
      </tui-segmented>
      @if (!index()) {
        <div tuiNotification appearance="info">
          WireGuard must first be installed on the client device and a key pair
          generated. The public key from this key pair will be used in this
          setup.
          <a tuiLink>Click here for device install instructions</a>
        </div>
        <tui-textfield>
          <label tuiLabel>Client Public Key</label>
          <input tuiInput formControlName="key" />
        </tui-textfield>
        <tui-error formControlName="key" />
        <div class="g-secondary">
          Download the config file below. In your client device configuration,
          paste the downloaded server configuration
          <b>
            replacing the private key placeholder with your client's private key
          </b>
        </div>
      } @else {
        <div>
          To connect to a client device, scan the QR code or use the
          configuration file.
        </div>
        <div tuiNotification appearance="warning">
          <b>Note:</b>
          Multiple WireGuard clients should not share the same VPN configuration
          file for security reasons.
        </div>
        <qr-code size="180" [value]="form.value.key || 'test'" />
      }
      <footer>
        <a tuiButton appearance="outline" iconStart="@tui.download">
          Download File
        </a>
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
  styles: `
    tui-segmented button {
      flex: 1;
    }

    qr-code {
      margin: 0 auto;
    }

    footer a {
      margin-inline-end: auto;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    ReactiveFormsModule,
    QrCodeComponent,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiButton,
    TuiSegmented,
    TuiNotification,
    TuiLink,
    TuiInput,
    ModalHelp,
    InboundClientsDialogAside,
  ],
})
class AddClient {
  protected readonly context =
    injectContext<TuiDialogContext<Client, Client | undefined>>()

  protected readonly index = signal(1)
  protected readonly form = inject(NonNullableFormBuilder).group({
    name: [this.context.data?.name || '', Validators.required],
    address: [this.context.data?.address || '', Validators.required],
    key: [this.context.data?.key || '', Validators.required],
  })

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      this.context.completeWith(this.form.getRawValue())
    }
  }
}

export const ADD_CLIENT = new PolymorpheusComponent(AddClient)
