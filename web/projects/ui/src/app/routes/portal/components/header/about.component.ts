import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { CopyService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiFade } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
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
          <strong>Git Hash</strong>
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
          <strong>CA fingerprint</strong>
          <div tuiSubtitle tuiFade>{{ server.caFingerprint }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(server.caFingerprint)"
        >
          {{ 'Copy' | i18n }}
        </button>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>Public Key</strong>
          <div tuiSubtitle tuiFade>{{ server.pubkey }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(server.pubkey)"
        >
          {{ 'Copy' | i18n }}
        </button>
      </div>
    }
  `,
  styles: '[tuiCell] { padding-inline: 0; white-space: nowrap }',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTitle, TuiButton, TuiCell, i18nPipe, TuiFade],
})
export class AboutComponent {
  readonly copyService = inject(CopyService)
  readonly gitHash = inject(ConfigService).gitHash
  readonly server = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo'),
  )
}

export const ABOUT = new PolymorpheusComponent(AboutComponent)
