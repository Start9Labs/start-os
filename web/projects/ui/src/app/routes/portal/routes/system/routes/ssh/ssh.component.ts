import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  viewChild,
} from '@angular/core'
import {
  ErrorService,
  DocsLinkDirective,
  i18nPipe,
  i18nKey,
  LoadingService,
} from '@start9labs/shared'
import { TuiHeader } from '@taiga-ui/layout'
import { from, merge, Subject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SSHTableComponent } from './table.component'
import { SSHKey } from 'src/app/services/api/api.types'
import { TuiLet } from '@taiga-ui/cdk'
import { ISB } from '@start9labs/start-sdk'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { FormDialogService } from 'src/app/services/form-dialog.service'

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
            'Connecting an external SMTP server allows StartOS and your installed services to send you emails.'
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
    <section *tuiLet="keys$ | async as keys" class="g-card">
      <header>
        Saved Keys
        <div [style.margin-inline-start]="'auto'">
          <button
            [style.margin-right]="'1rem'"
            tuiButton
            size="xs"
            appearance="primary-destructive"
            [disabled]="!tableKeys()?.selected()?.length"
            (click)="remove(keys || [])"
          >
            <!-- @TODO add translation -->
            {{ 'Delete selected' }}
          </button>
          <button
            tuiButton
            size="xs"
            iconStart="@tui.plus"
            (click)="add(keys || [])"
          >
            Add Key
          </button>
        </div>
      </header>
      <div #table [keys]="keys$ | async"></div>
    </section>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    SSHTableComponent,
    RouterLink,
    TitleDirective,
    TuiTable,
    TuiHeader,
    TuiTitle,
    TuiLink,
    i18nPipe,
    DocsLinkDirective,
    TuiLet,
  ],
})
export default class SystemSSHComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)

  private readonly local$ = new Subject<readonly SSHKey[]>()

  readonly keys$ = merge(from(this.api.getSshKeys({})), this.local$)

  protected tableKeys = viewChild<SSHTableComponent<SSHKey>>('table')

  async add(all: readonly SSHKey[]) {
    this.formDialog.open(FormComponent, {
      label: 'Add SSH Public Key' as i18nKey, // @TODO add translation
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

  async remove(all: readonly SSHKey[]) {
    const fingerprints =
      this.tableKeys()
        ?.selected()
        .map(s => s.fingerprint) || []

    const loader = this.loader.open('Deleting').subscribe()

    try {
      await this.api.deleteSshKey({ fingerprint: '' })
      this.local$.next(all.filter(s => !fingerprints.includes(s.fingerprint)))
      this.tableKeys()?.selected.set([])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
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
