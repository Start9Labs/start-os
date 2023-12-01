import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  FormsModule,
  ReactiveFormsModule,
  UntypedFormGroup,
} from '@angular/forms'
import { TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { customSmtp } from '@start9labs/start-sdk/lib/config/configConstants'
import { PatchDB } from 'patch-db-client'
import { switchMap } from 'rxjs'
import { FormModule } from 'src/app/common/form/form.module'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { EmailInfoComponent } from './info.component'

@Component({
  template: `
    <email-info />
    <ng-container *ngIf="form$ | async as form">
      <form [formGroup]="form">
        <h3 class="g-title">SMTP Credentials</h3>
        <form-group
          *ngIf="spec | async as resolved"
          [spec]="resolved"
        ></form-group>
        <div class="ion-text-right ion-padding-top">
          <button
            tuiButton
            size="m"
            [disabled]="form.invalid"
            (click)="save(form.value)"
          >
            Save
          </button>
        </div>
      </form>
      <form>
        <h3 class="g-title">Test Email</h3>
        <tui-input
          [(ngModel)]="testAddress"
          [ngModelOptions]="{ standalone: true }"
        >
          Firstname Lastname &lt;email@example.com&gt;
          <input tuiTextfield inputmode="email" />
        </tui-input>
        <div class="ion-text-right ion-padding-top">
          <button
            tuiButton
            appearance="secondary"
            size="m"
            [disabled]="!testAddress || form.invalid"
            (click)="sendTestEmail(form)"
          >
            Send Test Email
          </button>
        </div>
      </form>
    </ng-container>
  `,
  styles: ['form { margin: auto; max-width: 30rem; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    FormModule,
    TuiButtonModule,
    TuiInputModule,
    EmailInfoComponent,
  ],
})
export class SettingsEmailComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly api = inject(ApiService)

  testAddress = ''
  readonly spec: Promise<InputSpec> = configBuilderToSpec(customSmtp)
  readonly form$ = this.patch
    .watch$('server-info', 'smtp')
    .pipe(
      switchMap(async value =>
        this.formService.createForm(await this.spec, value),
      ),
    )

  async save(value: unknown): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.configureEmail(customSmtp.validator.unsafeCast(value))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async sendTestEmail(form: UntypedFormGroup) {
    const loader = this.loader.open('Sending...').subscribe()

    try {
      await this.api.testEmail({
        to: this.testAddress,
        ...form.value,
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
