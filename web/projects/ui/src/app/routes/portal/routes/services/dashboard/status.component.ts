import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'
import { getProgressText } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  PrimaryRendering,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'app-status',
  template: `
    @if (error()) {
      <tui-icon icon="@tui.triangle-alert" class="g-warning" />
    } @else if (loading()) {
      <tui-loader size="s" />
    }

    <b [style.color]="color()">{{ statusText() | i18n }}</b>

    @if (showDots()) {
      <span class="loading-dots g-info"></span>
    }
  `,
  styles: `
    :host {
      display: flex;
      align-items: center;
      gap: 0.25rem;
      white-space: nowrap;
    }

    :host-context(tui-root._mobile) {
      tui-icon {
        font-size: 1rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, i18nPipe, TuiLoader],
})
export class StatusComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly hasDepErrors = input<boolean>(false)

  private readonly i18n = inject(i18nPipe)

  readonly status = computed((pkg = this.pkg()) => renderPkgStatus(pkg))

  readonly statusText = computed(
    (pkg = this.pkg(), { primary } = this.status()) =>
      pkg.stateInfo.installingInfo
        ? (`${this.i18n.transform('Installing')}... ${this.i18n.transform(getProgressText(pkg.stateInfo.installingInfo.progress.overall))}` as i18nKey)
        : PrimaryRendering[primary].display,
  )

  readonly error = computed(
    ({ primary, health } = this.status()) =>
      this.hasDepErrors() ||
      primary === 'task-required' ||
      primary === 'error' ||
      health === 'failure',
  )

  readonly loading = computed(
    ({ primary, health } = this.status()) =>
      primary === 'running' && health === 'loading',
  )

  readonly showDots = computed(({ primary } = this.status()) =>
    [
      'starting',
      'stopping',
      'restarting',
      'installing',
      'updating',
      'backing-up',
      'removing',
    ].includes(primary),
  )

  readonly color = computed(({ primary } = this.status()) => {
    switch (primary) {
      case 'running':
        return 'var(--tui-status-positive)'
      case 'task-required':
        return 'var(--tui-status-warning)'
      case 'error':
        return 'var(--tui-status-negative)'
      case 'installing':
      case 'updating':
      case 'stopping':
      case 'starting':
      case 'backing-up':
      case 'restarting':
      case 'removing':
      case 'restoring':
        return 'var(--tui-status-info)'
      case 'stopped':
        return 'var(--tui-text-secondary)'
    }
  })
}
