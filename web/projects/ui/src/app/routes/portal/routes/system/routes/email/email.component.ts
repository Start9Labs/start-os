import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { inputSpec, IST } from '@start9labs/start-sdk'
import { TuiButton, TuiDialogService, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { TuiInputModule } from '@taiga-ui/legacy'
import { PatchDB } from 'patch-db-client'
import { switchMap, tap } from 'rxjs'
import { FormModule } from 'src/app/routes/portal/components/form/form.module'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      Email
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>Email</h3>
        <p tuiSubtitle>
          Connect to an external SMTP server for sending emails. Adding SMTP
          credentials enables StartOS and some services to send you emails.
          <a
            tuiLink
            href="https://docs.start9.com/latest/user-manual/smtp"
            target="_blank"
            rel="noreferrer"
            appearance="action-grayscale"
            iconEnd="@tui.external-link"
            [pseudo]="true"
            [textContent]="'View instructions'"
          ></a>
        </p>
      </hgroup>
    </header>
    @if (form$ | async; as form) {
      <form [formGroup]="form">
        <header tuiHeader="body-l">
          <h3 tuiTitle><b>SMTP Credentials</b></h3>
        </header>
        @if (spec | async; as resolved) {
          <form-group [spec]="resolved" />
        }
        <footer>
          @if (isSaved) {
            <button
              tuiButton
              size="l"
              appearance="secondary-destructive"
              (click)="save(null)"
            >
              Delete
            </button>
          }
          <button
            tuiButton
            size="l"
            [disabled]="form.invalid"
            (click)="save(form.value)"
          >
            Save
          </button>
        </footer>
      </form>
      <form>
        <header tuiHeader="body-l">
          <h3 tuiTitle><b>Send Test Email</b></h3>
        </header>
        <tui-input
          [(ngModel)]="testAddress"
          [ngModelOptions]="{ standalone: true }"
        >
          Name Lastname &lt;email&#64;example.com&gt;
          <input tuiTextfieldLegacy inputmode="email" />
        </tui-input>
        <footer>
          <button
            tuiButton
            appearance="secondary"
            size="l"
            [disabled]="!testAddress || form.invalid"
            (click)="sendTestEmail(form.value)"
          >
            Send
          </button>
        </footer>
      </form>
    }
  `,
  styles: `
    :host {
      max-width: 40rem;
    }

    form header,
    form footer {
      margin: 1rem 0;
      display: flex;
      gap: 1rem;
    }

    footer {
      justify-content: flex-end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    FormModule,
    TuiButton,
    TuiInputModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    RouterLink,
    TitleDirective,
  ],
})
export default class SystemEmailComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)

  testAddress = ''
  isSaved = false

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
