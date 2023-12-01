import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { CopyService, EmverPipesModule } from '@start9labs/shared'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  template: `
    <ng-container *ngIf="server$ | async as server">
      <h3 class="g-title" [style.margin-top.rem]="1">StartOS Info</h3>
      <div tuiCell [style.box-shadow]="'0 1px var(--tui-clear)'">
        <div tuiTitle>
          <strong>Version</strong>
          <div tuiSubtitle>{{ server.version | displayEmver }}</div>
        </div>
      </div>
      <div tuiCell>
        <div tuiTitle>
          <strong>Git Hash</strong>
          <div tuiSubtitle>{{ server.version | displayEmver }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconLeft="tuiIconCopy"
          (click)="copyService.copy(gitHash)"
        >
          Copy
        </button>
      </div>

      <h3 class="g-title">Device Credentials</h3>
      <div tuiCell>
        <div tuiTitle>
          <strong>CA fingerprint</strong>
          <div tuiSubtitle>{{ server['ca-fingerprint'] }}</div>
        </div>
        <button
          tuiIconButton
          appearance="icon"
          iconLeft="tuiIconCopy"
          (click)="copyService.copy(server['ca-fingerprint'])"
        >
          Copy
        </button>
      </div>
    </ng-container>
  `,
  styles: ['[tuiCell] { padding-inline: 0 }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    EmverPipesModule,
    TuiTitleModule,
    TuiButtonModule,
    TuiCellModule,
  ],
})
export class SettingsSpecComponent {
  readonly server$ = inject(PatchDB<DataModel>).watch$('server-info')
  readonly copyService = inject(CopyService)
  readonly gitHash = inject(ConfigService).gitHash
}
