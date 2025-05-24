import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'
import { getProgressText } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'td[appStatus]',
  template: `
    @if (loading) {
      <tui-loader size="s" />
    } @else {
      @if (!healthy) {
        <tui-icon icon="@tui.triangle-alert" class="g-warning" />
      }
    }
    <b [style.color]="color">{{ status | i18n }}{{ dots }}</b>
  `,
  styles: `
    :host {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      height: 3rem;
      white-space: nowrap;
    }

    :host-context(tui-root._mobile) {
      height: auto;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiLoader, i18nPipe],
})
export class StatusComponent {
  @Input()
  pkg!: PackageDataEntry

  @Input()
  hasDepErrors = false

  private readonly i18n = inject(i18nPipe)

  get healthy(): boolean {
    const { primary, health } = this.getStatus(this.pkg)
    return !this.hasDepErrors && primary !== 'error' && health !== 'failure'
  }

  get loading(): boolean {
    return this.color === 'var(--tui-status-info)'
  }

  @tuiPure
  getStatus(pkg: PackageDataEntry) {
    return renderPkgStatus(pkg)
  }

  get status(): i18nKey {
    if (this.pkg.stateInfo.installingInfo) {
      return `${this.i18n.transform('Installing')}... ${this.i18n.transform(getProgressText(this.pkg.stateInfo.installingInfo.progress.overall))}` as i18nKey
    }

    switch (this.getStatus(this.pkg).primary) {
      case 'running':
        return 'Running'
      case 'stopped':
        return 'Stopped'
      case 'actionRequired':
        return 'Task Required'
      case 'updating':
        return 'Updating'
      case 'stopping':
        return 'Stopping'
      case 'starting':
        return 'Starting'
      case 'backingUp':
        return 'Backing Up'
      case 'restarting':
        return 'Restarting'
      case 'removing':
        return 'Removing'
      case 'restoring':
        return 'Restoring'
      case 'error':
        return 'Error'
      default:
        return 'Unknown'
    }
  }

  get dots(): '...' | '' {
    switch (this.getStatus(this.pkg).primary) {
      case 'updating':
      case 'stopping':
      case 'starting':
      case 'backingUp':
      case 'restarting':
      case 'removing':
        return '...'
      default:
        return ''
    }
  }

  get color(): string {
    switch (this.getStatus(this.pkg).primary) {
      case 'running':
        return 'var(--tui-status-positive)'
      case 'actionRequired':
        return 'var(--tui-status-warning)'
      case 'error':
        return 'var(--tui-status-negative)'
      case 'installing':
      case 'updating':
      case 'stopping':
      case 'starting':
      case 'backingUp':
      case 'restarting':
      case 'removing':
      case 'restoring':
        return 'var(--tui-status-info)'
      // stopped
      default:
        return 'var(--tui-text-secondary)'
    }
  }
}
