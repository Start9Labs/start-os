import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import {
  FormControl,
  FormGroup,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogService,
  TuiError,
  TuiIcon,
  TuiTextfield,
} from '@taiga-ui/core'
import {
  TUI_VALIDATION_ERRORS,
  TuiButtonLoading,
  TuiFieldErrorPipe,
  TuiPassword,
} from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from '../services/api.service'
import { StartOSDiskInfoWithId } from '../types'

export interface CifsResult {
  cifs: T.Cifs
  servers: StartOSDiskInfoWithId[]
}

@Component({
  template: `
    <form [formGroup]="form" (ngSubmit)="submit()">
      <tui-textfield>
        <label tuiLabel>{{ 'Hostname' | i18n }}*</label>
        <input
          tuiTextfield
          formControlName="hostname"
          placeholder="e.g. 'My Computer' OR 'my-computer.local'"
        />
      </tui-textfield>
      <tui-error
        formControlName="hostname"
        [error]="['required'] | tuiFieldError | async"
      />

      <tui-textfield class="input">
        <label tuiLabel>{{ 'Path' | i18n }}*</label>
        <input
          tuiTextfield
          formControlName="path"
          placeholder="/Desktop/my-folder"
        />
      </tui-textfield>
      <tui-error formControlName="path" [error]="[] | tuiFieldError | async" />

      <tui-textfield class="input">
        <label tuiLabel>{{ 'Username' | i18n }}*</label>
        <input
          tuiTextfield
          formControlName="username"
          placeholder="Enter username"
        />
      </tui-textfield>
      <tui-error
        formControlName="username"
        [error]="[] | tuiFieldError | async"
      />

      <tui-textfield class="input">
        <label tuiLabel>{{ 'Password' | i18n }}</label>
        <input tuiTextfield type="password" formControlName="password" />
        <tui-icon tuiPassword />
      </tui-textfield>

      <footer>
        <button
          tuiButton
          appearance="secondary"
          type="button"
          [disabled]="connecting"
          (click)="cancel()"
        >
          {{ 'Cancel' | i18n }}
        </button>
        <button tuiButton [disabled]="form.invalid" [loading]="connecting">
          {{ 'Connect' | i18n }}
        </button>
      </footer>
    </form>
  `,
  styles: `
    .input {
      margin-top: 1rem;
    }

    footer {
      display: flex;
      gap: 1rem;
      margin-top: 1.5rem;
    }
  `,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    TuiButton,
    TuiButtonLoading,
    TuiTextfield,
    TuiPassword,
    TuiError,
    TuiFieldErrorPipe,
    TuiIcon,
    i18nPipe,
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
  private readonly context = injectContext<TuiDialogContext<CifsResult>>()
  private readonly i18n = inject(i18nPipe)

  connecting = false

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
    this.connecting = true

    try {
      const diskInfo = await this.api.verifyCifs({
        ...this.form.getRawValue(),
        password: this.form.value.password
          ? await this.api.encrypt(String(this.form.value.password))
          : null,
      })

      const servers = Object.keys(diskInfo).map(id => ({
        id,
        ...diskInfo[id]!,
      }))

      this.context.completeWith({
        cifs: { ...this.form.getRawValue() },
        servers,
      })
    } catch (e) {
      this.connecting = false
      this.onFail()
    }
  }

  private onFail() {
    this.dialogs
      .open(
        this.i18n.transform(
          'Unable to connect to network folder. Ensure (1) target computer is connected to LAN, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.',
        ),
        {
          label: this.i18n.transform('Connection Failed'),
          size: 's',
        },
      )
      .subscribe()
  }
}

export const CIFS = new PolymorpheusComponent(CifsComponent)
