import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { CopyService, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiHint, TuiTitle, TuiCell } from '@taiga-ui/core'
import { TuiFade } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    @if (server(); as server) {
      <div tuiCell>
        <div tuiTitle>
          <strong>{{ 'Version' | i18n }}</strong>
          <div tuiSubtitle>{{ server.version }}</div>
        </div>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>{{ 'Git hash' | i18n }}</strong>
          <div tuiSubtitle tuiFade>{{ gitHash }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(gitHash)"
        >
          {{ 'Copy' | i18n }}
        </button>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>{{ 'Root CA' | i18n }}</strong>
          <div tuiSubtitle tuiFade>{{ server.caFingerprint }}</div>
        </div>
        <a
          tuiIconButton
          download
          appearance="icon"
          iconStart="@tui.download"
          href="/static/local-root-ca.crt"
          [tuiHint]="'Download' | i18n"
          tuiHintDirection="bottom"
        >
          {{ 'Download' | i18n }}
        </a>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(server.caFingerprint)"
          [tuiHint]="'Copy fingerprint' | i18n"
          tuiHintDirection="bottom"
        >
          {{ 'Copy' | i18n }}
        </button>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>Public Key</strong>
          <div tuiSubtitle tuiFade>{{ getPubkey(server) }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(getPubkey(server))"
        >
          {{ 'Copy' | i18n }}
        </button>
      </div>
    }
  `,
  styles: '[tuiCell] { padding-inline: 0; white-space: nowrap }',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTitle, TuiButton, TuiCell, i18nPipe, TuiFade, TuiHint],
})
export class AboutComponent {
  readonly copyService = inject(CopyService)
  readonly gitHash = inject(ConfigService).gitHash
  readonly server = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo'),
  )

  getPubkey(server: T.ServerInfo) {
    return `${server.pubkey} startos@${server.hostname}`
  }
}

export const ABOUT = new PolymorpheusComponent(AboutComponent)
