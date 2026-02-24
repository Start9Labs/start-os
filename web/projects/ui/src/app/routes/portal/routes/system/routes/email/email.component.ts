import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
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
import { inputSpec } from '@start9labs/start-sdk'
import { TuiButton, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { Subscription, switchMap, tap } from 'rxjs'
import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

const PROVIDER_HINTS: Record<string, string> = {
  gmail:
    'Requires an App Password. Enable 2FA in your Google account, then generate an App Password.',
  ses: 'Use SMTP credentials (not IAM credentials). Update the host to match your SES region.',
  sendgrid:
    "Username is 'apikey' (literal). Password is your SendGrid API key.",
  mailgun: 'Use SMTP credentials from your Mailgun domain settings.',
  protonmail:
    'Requires a Proton for Business account. Use your Proton email as username.',
}

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
      {{ 'SMTP' | i18n }}
    </ng-container>
    @if (form$ | async; as form) {
      <form [formGroup]="form">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>
              {{ 'SMTP Credentials' | i18n }}
              <a
                tuiIconButton
                size="xs"
                docsLink
                path="/start-os/user-manual/smtp.html"
                appearance="icon"
                iconStart="@tui.external-link"
              >
                {{ 'Documentation' | i18n }}
              </a>
            </b>
          </h3>
        </header>
        @if (spec | async; as resolved) {
          <form-group [spec]="resolved" />
        }
        @if (providerHint()) {
          <p class="provider-hint">{{ providerHint() }}</p>
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
        <tui-textfield>
          <label tuiLabel>Name Lastname &lt;email&#64;example.com&gt;</label>
          <input
            tuiTextfield
            inputmode="email"
            [(ngModel)]="testAddress"
            [ngModelOptions]="{ standalone: true }"
          />
        </tui-textfield>
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

    .provider-hint {
      margin: 0.5rem 0 0;
      font-size: 0.85rem;
      opacity: 0.7;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    FormGroupComponent,
    TuiButton,
    TuiTextfield,
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
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly formService = inject(FormService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  readonly providerHint = signal('')
  private providerSub: Subscription | null = null

  testAddress = ''
  isSaved = false

  readonly spec = configBuilderToSpec(inputSpec.constants.systemSmtpSpec)

  readonly form$ = this.patch.watch$('serverInfo', 'smtp').pipe(
    tap(value => {
      this.isSaved = !!value
    }),
    switchMap(async value => {
      const spec = await this.spec
      const formData = value
        ? { provider: { selection: detectProviderKey(value.host), value } }
        : undefined
      const form = this.formService.createForm(spec, formData)

      // Watch provider selection for hints
      this.providerSub?.unsubscribe()
      const selectionCtrl = form.get('provider.selection')
      if (selectionCtrl) {
        this.providerHint.set(PROVIDER_HINTS[selectionCtrl.value] || '')
        this.providerSub = selectionCtrl.valueChanges.subscribe(key => {
          this.providerHint.set(PROVIDER_HINTS[key] || '')
        })
      }

      return form
    }),
  )

  async save(formValue: Record<string, any> | null): Promise<void> {
    const loader = this.loader.open('Saving').subscribe()

    try {
      if (formValue) {
        await this.api.setSmtp(formValue['provider'].value)
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

  async sendTestEmail(formValue: Record<string, any>) {
    const smtpValue = formValue['provider'].value
    const loader = this.loader.open('Sending email').subscribe()
    const success =
      `${this.i18n.transform('A test email has been sent to')} ${this.testAddress}. <i>${this.i18n.transform('Check your spam folder and mark as not spam.')}</i>` as i18nKey

    try {
      await this.api.testSmtp({
        ...smtpValue,
        password: smtpValue.password || '',
        to: this.testAddress,
      })
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
