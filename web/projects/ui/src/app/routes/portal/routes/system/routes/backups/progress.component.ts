import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nPipe, LeafProgressPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiTitle, TuiCell } from '@taiga-ui/core'
import { TuiAvatar, TuiProgress } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { take } from 'rxjs'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: '[backupProgress]',
  template: `
    <header>{{ 'Backup Progress' | i18n }}</header>
    @let bp = backupProgress();
    @if (bp) {
      @let overallLeaf = bp.overall | leafProgress;
      @let overallPct = overallLeaf | installingProgress;
      <div class="overall">
        <span class="label">{{ 'Overall' | i18n }}</span>
        <span class="value">
          @if (overallLeaf === true) {
            {{ 'complete' | i18n }}
          } @else {
            {{ overallPct }}%
          }
        </span>
        <progress
          tuiProgressBar
          size="m"
          [max]="100"
          [class.g-positive]="overallLeaf === true"
          [attr.value]="overallLeaf === true ? 100 : overallPct"
        ></progress>
      </div>
      @for (phase of bp.phases; track phase.name) {
        @let pkg = pkgs()?.[phase.name];
        @let leaf = phase.progress | leafProgress;
        @let percent = leaf | installingProgress;
        <div tuiCell>
          @if (pkg) {
            <span tuiAvatar appearance="action-grayscale">
              <img alt="" [src]="pkg.icon" />
            </span>
            <span tuiTitle>{{ (pkg | toManifest).title }}</span>
          } @else {
            <span tuiTitle>{{ $any(phase.name) | i18n }}</span>
          }
          <span class="status">
            @if (leaf === true) {
              <tui-icon icon="@tui.check" class="g-positive" />
              {{ 'complete' | i18n }}
            } @else if (leaf === null) {
              <tui-icon icon="@tui.clock" />
              {{ 'waiting' | i18n }}
            } @else {
              <span>{{ percent }}%</span>
            }
          </span>
          @if (leaf !== null && leaf !== true) {
            <progress
              class="row-progress"
              tuiProgressBar
              size="xs"
              [max]="100"
              [attr.value]="leaf === false ? undefined : percent"
            ></progress>
          }
        </div>
      }
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    .overall {
      display: grid;
      grid-template-columns: 1fr auto;
      gap: 0.25rem 0.5rem;
      padding: 0.5rem 0 0.75rem;
    }

    .overall .value {
      text-align: end;
    }

    .overall progress {
      grid-column: span 2;
    }

    .row-progress {
      grid-column: 1 / -1;
      margin-top: 0.25rem;
    }

    .status {
      display: flex;
      align-items: center;
      gap: 0.25rem;
      margin-inline-start: auto;
      white-space: nowrap;
    }

    .status tui-icon {
      font-size: 1rem;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiIcon,
    TuiProgress,
    ToManifestPipe,
    LeafProgressPipe,
    InstallingProgressPipe,
    i18nPipe,
  ],
})
export class BackupProgressComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgs = toSignal(this.patch.watch$('packageData').pipe(take(1)))
  readonly backupProgress = toSignal(
    this.patch.watch$('serverInfo', 'statusInfo', 'backupProgress'),
  )
}
