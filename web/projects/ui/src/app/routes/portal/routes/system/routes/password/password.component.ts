import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import * as argon2 from '@start9labs/argon2'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import {
  TuiAlertService,
  TuiButton,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { from } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { getServerInfo } from 'src/app/utils/get-server-info'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      Change Password
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>Change Password</h3>
        <p tuiSubtitle>
          Change your StartOS master password.
          <strong>
            You will still need your current password to decrypt existing
            backups!
          </strong>
        </p>
      </hgroup>
    </header>
    @if (spec(); as spec) {
      <app-form [spec]="spec" [buttons]="buttons" />
    }
  `,
  styles: `
    :host {
      max-inline-size: 40rem;

      ::ng-deep footer {
        background: transparent !important;
        margin: 0;
      }
    }

    section {
      padding: 4rem 1rem 0;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    RouterLink,
    TuiHeader,
    TuiTitle,
    TuiButton,
    FormComponent,
    TitleDirective,
  ],
})
export default class SystemPasswordComponent {
  private readonly alerts = inject(TuiAlertService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  readonly spec = toSignal(from(configBuilderToSpec(passwordSpec)))
  readonly buttons = [
    {
      text: 'Save',
      handler: (value: PasswordSpec) => this.resetPassword(value),
    },
  ]

  private async resetPassword({
    newPassword,
    newPasswordConfirm,
    oldPassword,
  }: PasswordSpec) {
    let error = ''

    if (newPassword !== newPasswordConfirm) {
      error = 'New passwords do not match'
    } else if (newPassword.length < 12) {
      error = 'New password must be 12 characters or greater'
    } else if (newPassword.length > 64) {
      error = 'New password must be less than 65 characters'
    }

    // confirm current password is correct
    const { passwordHash } = await getServerInfo(this.patch)

    try {
      argon2.verify(passwordHash, oldPassword)
    } catch (e) {
      error = 'Current password is invalid'
    }

    if (error) {
      this.errorService.handleError(error)
      return
    }

    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.resetPassword({ oldPassword, newPassword })
      this.alerts.open('Password changed!').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

const passwordSpec = ISB.InputSpec.of({
  oldPassword: ISB.Value.text({
    name: 'Current Password',
    required: true,
    default: null,
    masked: true,
  }),
  newPassword: ISB.Value.text({
    name: 'New Password',
    required: true,
    default: null,
    masked: true,
  }),
  newPasswordConfirm: ISB.Value.text({
    name: 'Retype New Password',
    required: true,
    default: null,
    masked: true,
  }),
})

export type PasswordSpec = typeof passwordSpec.validator._TYPE
