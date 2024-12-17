import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { IST, inputSpec } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { switchMap, tap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'

@Component({
  selector: 'smtp-page',
  templateUrl: './smtp.page.html',
  styleUrls: ['./smtp.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SMTPPage {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  isSaved = false
  testAddress = ''

  readonly spec: Promise<IST.InputSpec> = configBuilderToSpec(
    inputSpec.constants.customSmtp,
  )
  readonly form$ = this.patch.watch$('serverInfo', 'smtp').pipe(
    tap(value => (this.isSaved = !!value)),
    switchMap(async value =>
      this.formService.createForm(await this.spec, value),
    ),
  )

  async save(
    value: typeof inputSpec.constants.customSmtp._TYPE | null,
  ): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      if (value) {
        await this.api.setSmtp(value)
        this.isSaved = true
      } else {
        await this.api.clearSmtp({})
        this.isSaved = false
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async sendTestEmail(value: typeof inputSpec.constants.customSmtp._TYPE) {
    const loader = this.loader.open('Sending email...').subscribe()

    try {
      await this.api.testSmtp({
        to: this.testAddress,
        ...value,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }

    this.dialogs
      .open(
        `A test email has been sent to ${this.testAddress}.<br /><br /><b>Check your spam folder and mark as not spam</b>`,
        {
          label: 'Success',
          size: 's',
        },
      )
      .subscribe()
  }
}
