import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormControl, ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  DialogService,
  DocsLinkDirective,
  ErrorService,
  i18nKey,
  i18nPipe,
} from '@start9labs/shared'
import { inputSpec, ISB, utils } from '@start9labs/start-sdk'
import { TuiButton, TuiError, TuiInput, TuiTitle } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { switchMap } from 'rxjs'
import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

function detectProviderKey(host: string | undefined): string {
  if (!host) return 'other'
  const providers: Record<string, string> = {
    'smtp.gmail.com': 'gmail',
    'smtp.sendgrid.net': 'sendgrid',
    'smtp.mailgun.org': 'mailgun',
    'smtp.protonmail.ch': 'protonmail',
  }
  for (const [h, key] of Object.entries(providers)) {
    if (host === h) return key
  }
  if (host.endsWith('.amazonaws.com')) return 'ses'
  return 'other'
}

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      SMTP
    </ng-container>
    @if (form$ | async; as data) {
      <form [formGroup]="data.form">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>
              SMTP
              <a
                tuiIconButton
                size="xs"
                docsLink
                path="/start-os/smtp.html"
                appearance="icon"
                iconStart="@tui.book-open-text"
              >
                {{ 'Documentation' | i18n }}
              </a>
            </b>
          </h3>
        </header>
        <form-group [spec]="data.spec" />
        <footer>
          @if (!data.form.pristine) {
            <button
              tuiButton
              size="l"
              appearance="secondary"
              (click)="cancel(data)"
            >
              {{ 'Cancel' | i18n }}
            </button>
          }
          <button
            tuiButton
            size="l"
            [disabled]="data.form.invalid || data.form.pristine"
            (click)="save(data.form.value)"
          >
            {{ 'Save' | i18n }}
          </button>
        </footer>
      </form>
      @if (data.form.value.smtp?.selection === 'enabled') {
        <form>
          <header tuiHeader="body-l">
            <h3 tuiTitle>
              <b>{{ 'Send test email' | i18n }}</b>
            </h3>
          </header>
          <tui-textfield>
            <label tuiLabel>email&#64;example.com</label>
            <input
              tuiInput
              inputmode="email"
              [formControl]="testEmailControl"
            />
          </tui-textfield>
          <tui-error
            [error]="
              !testEmailControl.pristine && isEmailInvalid
                ? ('Must be a valid email address' | i18n)
                : null
            "
          />
          <footer>
            <button
              tuiButton
              size="l"
              [disabled]="
                !testEmailControl.value || isEmailInvalid || data.form.invalid
              "
              (click)="sendTestEmail(data.form.value)"
            >
              {{ 'Send' | i18n }}
            </button>
          </footer>
        </form>
      }
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
    ReactiveFormsModule,
    FormGroupComponent,
    TuiButton,
    TuiError,
    TuiInput,
    TuiHeader,
    TuiTitle,
    RouterLink,
    TitleDirective,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemEmailComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  private readonly emailRegex = new RegExp(utils.Patterns.email.regex)
  readonly testEmailControl = new FormControl('')

  get isEmailInvalid(): boolean {
    const value = this.testEmailControl.value
    return !!value && !this.emailRegex.test(value)
  }

  private readonly smtpSpec = ISB.InputSpec.of({
    smtp: ISB.Value.union({
      name: this.i18n.transform('SMTP'),
      default: 'disabled',
      variants: ISB.Variants.of({
        disabled: {
          name: this.i18n.transform('Disabled'),
          spec: ISB.InputSpec.of({}),
        },
        enabled: {
          name: this.i18n.transform('Enabled'),
          spec: inputSpec.constants.systemSmtpSpec,
        },
      }),
    }),
  })

  readonly form$ = this.patch.watch$('serverInfo', 'smtp').pipe(
    switchMap(async value => {
      const spec = await configBuilderToSpec(this.smtpSpec)

      const formData = value
        ? {
            smtp: {
              selection: 'enabled' as const,
              value: {
                provider: {
                  selection: detectProviderKey(value.host),
                  value: {
                    host: value.host,
                    security: {
                      selection: value.security,
                      value: { port: String(value.port) },
                    },
                    from: value.from,
                    username: value.username,
                    password: value.password,
                  },
                },
              },
            },
          }
        : undefined
      const form = this.formService.createForm(spec, formData)

      return { form, spec, formData }
    }),
  )

  private getSmtpValue(formValue: Record<string, any>) {
    const { security, ...rest } = formValue['smtp'].value.provider.value
    return {
      ...rest,
      security: security.selection,
      port: Number(security.value.port),
    }
  }

  async save(formValue: Record<string, any>): Promise<void> {
    const loader = this.loader.open('Saving').subscribe()

    try {
      if (formValue['smtp'].selection === 'disabled') {
        await this.api.clearSmtp({})
      } else {
        await this.api.setSmtp(this.getSmtpValue(formValue))
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  cancel(data: {
    form: ReturnType<FormService['createForm']>
    formData: Record<string, any> | undefined
  }) {
    data.form.reset(data.formData)
  }

  async sendTestEmail(formValue: Record<string, any>) {
    const smtpValue = this.getSmtpValue(formValue)
    const address = this.testEmailControl.value!
    const loader = this.loader.open('Sending email').subscribe()
    const success =
      `${this.i18n.transform('A test email has been sent to')} ${address}. <i>${this.i18n.transform('Check your spam folder and mark as not spam.')}</i>` as i18nKey

    try {
      await this.api.testSmtp({
        ...smtpValue,
        password: smtpValue.password || '',
        to: address,
      })
      this.dialog
        .openAlert(success, { label: 'Success', size: 's' })
        .subscribe()
      this.testEmailControl.reset()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
