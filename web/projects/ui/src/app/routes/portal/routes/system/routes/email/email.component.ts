import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  DialogService,
  DocsLinkDirective,
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { inputSpec, IST } from '@start9labs/start-sdk'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
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
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'Email' | i18n }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ 'Email' | i18n }}</h3>
        <p tuiSubtitle>
          {{
            'Connecting an external SMTP server allows StartOS and your installed services to send you emails.'
              | i18n
          }}
          <a
            tuiLink
            docsLink
            href="/user-manual/smtp"
            appearance="action-grayscale"
            iconEnd="@tui.external-link"
            [pseudo]="true"
            [textContent]="'View instructions' | i18n"
          ></a>
        </p>
      </hgroup>
    </header>
    @if (form$ | async; as form) {
      <form [formGroup]="form">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>{{ 'SMTP Credentials' | i18n }}</b>
          </h3>
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
              {{ 'Delete' | i18n }}
            </button>
          }
          <button
            tuiButton
            size="l"
            [disabled]="form.invalid || form.pristine"
            (click)="save(form.value)"
          >
            {{ 'Save' | i18n }}
          </button>
        </footer>
      </form>
      <form>
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>{{ 'Send test email' | i18n }}</b>
          </h3>
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
            size="l"
            [disabled]="!testAddress || form.invalid"
            (click)="sendTestEmail(form.value)"
          >
            {{ 'Send' | i18n }}
          </button>
        </footer>
      </form>
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
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
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemEmailComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

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
    const loader = this.loader.open('Saving').subscribe()

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
    const loader = this.loader.open('Sending email').subscribe()
    const success =
      `${this.i18n.transform('A test email has been sent to')} ${this.testAddress}. <i>${this.i18n.transform('Check your spam folder and mark as not spam.')}</i>` as i18nKey

    try {
      await this.api.testSmtp({ to: this.testAddress, ...value })
      this.dialog
        .openAlert(success, { label: 'Success', size: 's' })
        .subscribe()
      this.testAddress = ''
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
