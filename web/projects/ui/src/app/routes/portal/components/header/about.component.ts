import { TuiCell } from '@taiga-ui/layout'
import { TuiTitle, TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  template: `
    <ng-container *ngIf="server$ | async as server">
      <div tuiCell>
        <div tuiTitle>
          <strong>Version</strong>
          <div tuiSubtitle>{{ server.version }}</div>
        </div>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>Git Hash</strong>
          <div tuiSubtitle>{{ gitHash }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(gitHash)"
        >
          Copy
        </button>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>CA fingerprint</strong>
          <div tuiSubtitle>{{ server.caFingerprint }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconStart="@tui.copy"
          (click)="copyService.copy(server.caFingerprint)"
        >
          Copy
        </button>
      </div>
    </ng-container>
  `,
  styles: ['[tuiCell] { padding-inline: 0 }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiTitle, TuiButton, TuiCell],
})
export class AboutComponent {
  readonly server$ = inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo')
  readonly copyService = inject(CopyService)
  readonly gitHash = inject(ConfigService).gitHash
}

export const ABOUT = new PolymorpheusComponent(AboutComponent)
