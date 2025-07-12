import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  DialogService,
  DocsLinkDirective,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { filter, from, merge, Subject } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { SSHKey } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { SSHTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      SSH
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>SSH</h3>
        <p tuiSubtitle>
          {{
            'By default, you can SSH into your server from any device using your master password. Optionally add SSH public keys to grant specific devices access without needing to enter a password.'
              | i18n
          }}
          <a
            tuiLink
            docsLink
            href="/user-manual/ssh"
            appearance="action-grayscale"
            iconEnd="@tui.external-link"
            [pseudo]="true"
            [textContent]="'View instructions' | i18n"
          ></a>
        </p>
      </hgroup>
    </header>
    @let keys = keys$ | async;
    <section class="g-card">
      <header>
        Saved Keys
        <button
          tuiButton
          size="xs"
          iconStart="@tui.trash"
          appearance="primary-destructive"
          [style.margin]="'0 0.5rem 0 auto'"
          [disabled]="!tableKeys()?.selected()?.length"
          (click)="remove(keys || [])"
        >
          {{ 'Delete selected' | i18n }}
        </button>
        <button
          tuiButton
          size="xs"
          iconStart="@tui.plus"
          (click)="add(keys || [])"
        >
          Add Key
        </button>
      </header>
      <div #table [keys]="keys"></div>
    </section>
  `,
  styles: `
    :host-context(tui-root._mobile) {
      [tuiButton] {
        font-size: 0;
        gap: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    SSHTableComponent,
    RouterLink,
    TitleDirective,
    TuiHeader,
    TuiTitle,
    TuiLink,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export default class SystemSSHComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)
  private readonly dialogs = inject(DialogService)

  private readonly local$ = new Subject<readonly SSHKey[]>()

  readonly keys$ = merge(from(this.api.getSshKeys({})), this.local$)

  protected tableKeys = viewChild<SSHTableComponent<SSHKey>>('table')

  async add(all: readonly SSHKey[]) {
    this.formDialog.open(FormComponent, {
      label: 'Add SSH Public Key',
      data: {
        spec: await configBuilderToSpec(SSHSpec),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async ({ key }: typeof SSHSpec._TYPE) => {
              const loader = this.loader.open('Saving').subscribe()

              try {
                const newKey = await this.api.addSshKey({ key })
                this.local$.next([newKey, ...all])
                return true
              } catch (e: any) {
                this.errorService.handleError(e)
                return false
              } finally {
                loader.unsubscribe()
              }
            },
          },
        ],
      },
    })
  }

  remove(all: readonly SSHKey[]) {
    this.dialogs
      .openConfirm({ label: 'Are you sure?', size: 's' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const selected = this.tableKeys()?.selected() || []
        const fingerprints = selected.map(s => s.fingerprint) || []
        const loader = this.loader.open('Deleting').subscribe()

        try {
          await this.api.deleteSshKey({ fingerprint: '' })
          this.local$.next(
            all.filter(s => !fingerprints.includes(s.fingerprint)),
          )
          this.tableKeys()?.selected.set([])
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}

const SSHSpec = ISB.InputSpec.of({
  key: ISB.Value.text({
    name: 'Public Key',
    required: true,
    default: null,
    patterns: [
      {
        regex:
          '^(ssh-(rsa|ed25519|dss|ecdsa)|ecdsa-sha2-nistp(256|384|521))\\s+[A-Za-z0-9+/=]+(\\s[^\\s]+)?$',
        description: 'must be a valid SSH public key',
      },
    ],
  }),
})
