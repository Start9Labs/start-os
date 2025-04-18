import { AsyncPipe, KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nPipe } from '@start9labs/shared'
import { TuiMapperPipe } from '@taiga-ui/cdk'
import { TuiIcon, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { take } from 'rxjs'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: '[backupProgress]',
  template: `
    <header>{{ 'Backup Progress' | i18n }}</header>
    @for (pkg of pkgs() | keyvalue; track $index) {
      @if (backupProgress()?.[pkg.key]; as progress) {
        <div tuiCell>
          <tui-avatar>
            <img alt="" [src]="pkg.value.icon" />
          </tui-avatar>
          <span tuiTitle>
            {{ (pkg.value | toManifest).title }}
            <span tuiSubtitle>
              @if (progress.complete) {
                <tui-icon icon="@tui.check" class="g-positive" />
                {{ 'Complete' | i18n }}
              } @else {
                @if ((pkg.key | tuiMapper: toStatus | async) === 'backingUp') {
                  <tui-loader size="s" />
                  {{ 'Backing up' | i18n }}
                } @else {
                  {{ 'Waiting' | i18n }}...
                }
              }
            </span>
          </span>
        </div>
      }
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    KeyValuePipe,
    AsyncPipe,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiIcon,
    TuiLoader,
    TuiMapperPipe,
    ToManifestPipe,
    i18nPipe,
  ],
})
export class BackupProgressComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgs = toSignal(this.patch.watch$('packageData').pipe(take(1)))
  readonly backupProgress = toSignal(
    this.patch.watch$('serverInfo', 'statusInfo', 'backupProgress'),
  )

  readonly toStatus = (pkgId: string) =>
    this.patch.watch$('packageData', pkgId, 'status', 'main')
}
