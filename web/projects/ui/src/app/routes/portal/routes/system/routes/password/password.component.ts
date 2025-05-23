import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import * as argon2 from '@start9labs/argon2'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
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
      {{ 'Change Password' | i18n }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ 'Change Password' | i18n }}</h3>
        <p tuiSubtitle>
          {{ 'Change your StartOS master password.' | i18n }}
          <strong>
            {{
              'You will still need your current password to decrypt existing backups!'
                | i18n
            }}
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
    i18nPipe,
  ],
})
export default class SystemPasswordComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  readonly spec = toSignal(from(configBuilderToSpec(this.passwordSpec())))
  readonly buttons = [
    {
      text: this.i18n.transform('Save')!,
      handler: (value: ReturnType<typeof this.passwordSpec>['_TYPE']) =>
        this.resetPassword(value),
    },
  ]

  private async resetPassword({
    newPassword,
    newPasswordConfirm,
    oldPassword,
  }: ReturnType<typeof this.passwordSpec>['_TYPE']) {
    let error: i18nKey | null = null

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

    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.resetPassword({ oldPassword, newPassword })
      this.dialog.openAlert('Password changed').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  passwordSpec() {
    return ISB.InputSpec.of({
      oldPassword: ISB.Value.text({
        name: this.i18n.transform('Current Password')!,
        required: true,
        default: null,
        masked: true,
      }),
      newPassword: ISB.Value.text({
        name: this.i18n.transform('New Password')!,
        required: true,
        default: null,
        masked: true,
      }),
      newPasswordConfirm: ISB.Value.text({
        name: this.i18n.transform('Retype New Password')!,
        required: true,
        default: null,
        masked: true,
      }),
    })
  }
}
