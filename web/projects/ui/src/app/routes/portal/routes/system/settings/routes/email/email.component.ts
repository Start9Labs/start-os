import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  FormsModule,
  ReactiveFormsModule,
  UntypedFormGroup,
} from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { config, CT } from '@start9labs/start-sdk'
import { TuiButton, TuiDialogService } from '@taiga-ui/core'
import { TuiInputModule } from '@taiga-ui/legacy'
import { PatchDB } from 'patch-db-client'
import { switchMap } from 'rxjs'
import { FormModule } from 'src/app/routes/portal/components/form/form.module'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { EmailInfoComponent } from './info.component'

@Component({
  template: `
    <email-info />
    <ng-container *ngIf="form$ | async as form">
      <form [formGroup]="form" [style.text-align]="'right'">
        <h3 class="g-title">SMTP Credentials</h3>
        <form-group
          *ngIf="spec | async as resolved"
          [spec]="resolved"
        ></form-group>
        <button
          tuiButton
          [style.margin-top.rem]="1"
          [disabled]="form.invalid"
          (click)="save(form.value)"
        >
          Save
        </button>
      </form>
      <form [style.text-align]="'right'">
        <h3 class="g-title">Test Email</h3>
        <tui-input
          [(ngModel)]="testAddress"
          [ngModelOptions]="{ standalone: true }"
        >
          Firstname Lastname &lt;email&#64;example.com&gt;
          <input tuiTextfieldLegacy inputmode="email" />
        </tui-input>
        <button
          tuiButton
          appearance="secondary"
          [style.margin-top.rem]="1"
          [disabled]="!testAddress || form.invalid"
          (click)="sendTestEmail(form)"
        >
          Send Test Email
        </button>
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
    TuiButton,
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
  readonly spec: Promise<CT.InputSpec> = configBuilderToSpec(
    config.constants.customSmtp,
  )
  readonly form$ = this.patch
    .watch$('serverInfo', 'smtp')
    .pipe(
      switchMap(async value =>
        this.formService.createForm(await this.spec, value),
      ),
    )

  async save(value: unknown): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.configureEmail(
        config.constants.customSmtp.validator.unsafeCast(value),
      )
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
