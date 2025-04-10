import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'
import { getProgressText } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  standalone: true,
  selector: 'td[appStatus]',
  template: `
    @if (loading) {
      <tui-loader size="s" />
    } @else {
      @if (healthy) {
        <tui-icon icon="@tui.check" class="g-positive" />
      } @else {
        <tui-icon icon="@tui.triangle-alert" class="g-warning" />
      }
    }
    <b [style.color]="color">{{ status }}</b>
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

      tui-loader,
      tui-icon {
        position: absolute;
        top: 1rem;
        left: 1rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiLoader],
})
export class StatusComponent {
  @Input()
  pkg!: PackageDataEntry

  @Input()
  hasDepErrors = false

  get healthy(): boolean {
    return !this.hasDepErrors && this.getStatus(this.pkg).health !== 'failure'
  }

  get loading(): boolean {
    return this.color === 'var(--tui-status-info)'
  }

  @tuiPure
  getStatus(pkg: PackageDataEntry) {
    return renderPkgStatus(pkg, {})
  }

  get status(): string {
    if (this.pkg.stateInfo.installingInfo) {
      return `Installing...${getProgressText(this.pkg.stateInfo.installingInfo.progress.overall)}`
    }

    switch (this.getStatus(this.pkg).primary) {
      case 'running':
        return 'Running'
      case 'stopped':
        return 'Stopped'
      case 'actionRequired':
        return 'Action Required'
      case 'updating':
        return 'Updating...'
      case 'stopping':
        return 'Stopping...'
      case 'starting':
        return 'Starting...'
      case 'backingUp':
        return 'Backing Up...'
      case 'restarting':
        return 'Restarting...'
      case 'removing':
        return 'Removing...'
      case 'restoring':
        return 'Restoring...'
      default:
        return 'Unknown'
    }
  }

  get color(): string {
    switch (this.getStatus(this.pkg).primary) {
      case 'running':
        return 'var(--tui-status-positive)'
      case 'actionRequired':
        return 'var(--tui-status-warning)'
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
