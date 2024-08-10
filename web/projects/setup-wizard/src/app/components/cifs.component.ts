import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import {
  FormControl,
  FormGroup,
  FormsModule,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { LoadingService, StartOSDiskInfo } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogService,
  TuiError,
} from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiInputModule, TuiInputPasswordModule } from '@taiga-ui/legacy'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { SERVERS, ServersResponse } from 'src/app/components/servers.component'
import { ApiService } from 'src/app/services/api.service'

export interface CifsResponse {
  cifs: T.Cifs
  serverId: string
  password: string
}

@Component({
  standalone: true,
  template: `
    <form [formGroup]="form" (ngSubmit)="submit()">
      <tui-input formControlName="hostname">
        Hostname *
        <input
          tuiTextfieldLegacy
          placeholder="e.g. 'My Computer' OR 'my-computer.local'"
        />
      </tui-input>
      <tui-error
        formControlName="hostname"
        [error]="['required'] | tuiFieldError | async"
      ></tui-error>

      <tui-input formControlName="path" class="input">
        Path *
        <input tuiTextfieldLegacy placeholder="/Desktop/my-folder'" />
      </tui-input>
      <tui-error
        formControlName="path"
        [error]="[] | tuiFieldError | async"
      ></tui-error>

      <tui-input formControlName="username" class="input">
        Username *
        <input tuiTextfieldLegacy placeholder="Enter username" />
      </tui-input>
      <tui-error
        formControlName="username"
        [error]="[] | tuiFieldError | async"
      ></tui-error>

      <tui-input-password formControlName="password" class="input">
        Password
      </tui-input-password>

      <footer>
        <button
          tuiButton
          appearance="secondary"
          type="button"
          (click)="cancel()"
        >
          Cancel
        </button>
        <button tuiButton [disabled]="form.invalid">Verify</button>
      </footer>
    </form>
  `,
  styles: [
    '.input { margin-top: 1rem }',
    'footer { display: flex; gap: 1rem; margin-top: 1rem }',
  ],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TuiButton,
    TuiInputModule,
    TuiInputPasswordModule,
    TuiError,
    TuiFieldErrorPipe,
  ],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: {
        required: 'This field is required',
      },
    },
  ],
})
export class CifsComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly context =
    inject<TuiDialogContext<CifsResponse>>(POLYMORPHEUS_CONTEXT)

  readonly form = new FormGroup({
    hostname: new FormControl('', {
      validators: [
        Validators.required,
        Validators.pattern(/^[a-zA-Z0-9._-]+( [a-zA-Z0-9]+)*$/),
      ],
      nonNullable: true,
    }),
    path: new FormControl('', {
      validators: [Validators.required],
      nonNullable: true,
    }),
    username: new FormControl('', {
      validators: [Validators.required],
      nonNullable: true,
    }),
    password: new FormControl(),
  })

  cancel() {
    this.context.$implicit.complete()
  }

  async submit(): Promise<void> {
    const loader = this.loader
      .open('Connecting to shared folder...')
      .subscribe()

    try {
      const diskInfo = await this.api.verifyCifs({
        ...this.form.getRawValue(),
        password: this.form.value.password
          ? await this.api.encrypt(String(this.form.value.password))
          : null,
      })

      loader.unsubscribe()

      this.selectServer(diskInfo)
    } catch (e) {
      loader.unsubscribe()
      this.onFail()
    }
  }

  private selectServer(servers: Record<string, StartOSDiskInfo>) {
    this.dialogs
      .open<ServersResponse>(SERVERS, {
        label: 'Select Server to Restore',
        data: {
          servers: Object.keys(servers).map(id => ({ id, ...servers[id] })),
        },
      })
      .subscribe(({ password, serverId }) => {
        this.context.completeWith({
          cifs: { ...this.form.getRawValue() },
          serverId,
          password,
        })
      })
  }

  private onFail() {
    this.dialogs
      .open(
        'Unable to connect to shared folder. Ensure (1) target computer is connected to LAN, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.',
        {
          label: 'Connection Failed',
          size: 's',
        },
      )
      .subscribe()
  }
}

export const CIFS = new PolymorpheusComponent(CifsComponent)
