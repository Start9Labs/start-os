import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormService } from 'src/app/services/form.service'
import { emailSpec } from './email.const'
import { LoadingService } from '../../../modals/loading/loading.service'
import { TuiDialogService } from '@taiga-ui/core'
import { RR } from '../../../services/api/api.types'
import { switchMap } from 'rxjs/operators'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'

@Component({
  selector: 'email',
  templateUrl: './email.page.html',
  styleUrls: ['./email.page.scss'],
})
export class EmailPage {
  spec: Promise<InputSpec> = configBuilderToSpec(emailSpec)
  readonly form$ = this.patch
    .watch$('server-info', 'email')
    .pipe(
      switchMap(async value =>
        this.formService.createForm(await this.spec, value),
      ),
    )

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly patch: PatchDB<DataModel>,
    private readonly api: ApiService,
    private readonly formService: FormService,
  ) {}

  async save(value: unknown): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.configureEmail(emailSpec.validator.unsafeCast(value))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  sendTestEmail({ address }: RR.ConfigureEmailReq) {
    this.dialogs
      .open(
        `A test email has been sent to ${address}.<br /><br /><b>Check your spam folder and mark as not spam</b>`,
        {
          label: 'Success',
          size: 's',
        },
      )
      .subscribe()
  }
}
