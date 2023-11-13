import { Component, Inject } from '@angular/core'
import { FormControl, FormGroup, Validators } from '@angular/forms'
import { TUI_VALIDATION_ERRORS } from '@taiga-ui/kit'
import { LoadingService, StartOSDiskInfo } from '@start9labs/shared'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import {
  ApiService,
  CifsBackupTarget,
  CifsRecoverySource,
} from 'src/app/services/api/api.service'
import { PASSWORD } from '../password/password.page'

@Component({
  selector: 'cifs-modal',
  templateUrl: 'cifs-modal.page.html',
  styleUrls: ['cifs-modal.page.scss'],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: {
        required: 'This field is required',
      },
    },
  ],
})
export class CifsModal {
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

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<{
      cifs: CifsRecoverySource
      recoveryPassword: string
    }>,
    private readonly dialogs: TuiDialogService,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
  ) {}

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
        type: 'cifs',
        password: this.form.value.password
          ? await this.api.encrypt(String(this.form.value.password))
          : null,
      })

      loader.unsubscribe()

      this.presentModalPassword(diskInfo)
    } catch (e) {
      loader.unsubscribe()
      this.presentAlertFailed()
    }
  }

  private presentModalPassword(diskInfo: StartOSDiskInfo) {
    const target: CifsBackupTarget = {
      ...this.form.getRawValue(),
      mountable: true,
      'embassy-os': diskInfo,
    }

    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Unlock Drive',
        size: 's',
        data: { target },
      })
      .subscribe(recoveryPassword => {
        this.context.completeWith({
          cifs: { ...this.form.getRawValue(), type: 'cifs' },
          recoveryPassword,
        })
      })
  }

  private presentAlertFailed() {
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
