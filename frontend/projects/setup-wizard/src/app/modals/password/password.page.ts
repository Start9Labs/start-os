import { Component, Inject } from '@angular/core'
import { FormControl } from '@angular/forms'
import * as argon2 from '@start9labs/argon2'
import { ErrorService } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import {
  PolymorpheusComponent,
  POLYMORPHEUS_CONTEXT,
} from '@tinkoff/ng-polymorpheus'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.service'

interface DialogData {
  target?: CifsBackupTarget | DiskBackupTarget
  storageDrive?: boolean
}

@Component({
  selector: 'app-password',
  templateUrl: 'password.page.html',
})
export class PasswordPage {
  readonly target = this.context.data.target
  readonly storageDrive = this.context.data.storageDrive
  readonly password = new FormControl('', { nonNullable: true })
  readonly confirm = new FormControl('', { nonNullable: true })

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<string, DialogData>,
    private readonly errorService: ErrorService,
  ) {}

  get passwordError(): string | null {
    if (!this.password.touched || this.target) return null

    if (!this.storageDrive && !this.target?.['embassy-os'])
      return 'No recovery target' // unreachable

    if (this.password.value.length < 12)
      return 'Must be 12 characters or greater'

    if (this.password.value.length > 64)
      return 'Must be less than 65 characters'

    return null
  }

  get confirmError(): string | null {
    return this.confirm.touched && this.password.value !== this.confirm.value
      ? 'Passwords do not match'
      : null
  }

  verifyPw() {
    try {
      const passwordHash = this.target!['embassy-os']?.['password-hash'] || ''

      argon2.verify(passwordHash, this.password.value)
      this.context.completeWith(this.password.value)
    } catch (e) {
      this.errorService.handleError('Incorrect password provided')
    }
  }

  submitPw() {
    this.context.completeWith(this.password.value)
  }

  cancel() {
    this.context.$implicit.complete()
  }
}

export const PASSWORD = new PolymorpheusComponent(PasswordPage)
