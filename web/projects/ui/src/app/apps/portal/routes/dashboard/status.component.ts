import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  HealthStatus,
  PrimaryStatus,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { InstallingProgressDisplayPipe } from '../service/pipes/install-progress.pipe'

@Component({
  standalone: true,
  selector: 'td[appStatus]',
  template: `
    @if (loading) {
      <tui-loader size="s" />
    } @else {
      @if (healthy) {
        <tui-icon icon="tuiIconCheck" class="g-success" />
      } @else {
        <tui-icon icon="tuiIconAlertTriangle" class="g-warning" />
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
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIconModule, TuiLoaderModule],
})
export class StatusComponent {
  @Input()
  pkg!: PackageDataEntry

  @Input()
  hasDepErrors = false

  get healthy(): boolean {
    const status = this.getStatus(this.pkg)

    return (
      !this.hasDepErrors && // no deps error
      !!this.pkg.status.configured && // no config needed
      status.health !== HealthStatus.Failure // no health issues
    )
  }

  get loading(): boolean {
    return !!this.pkg.stateInfo || this.color === 'var(--tui-info-fill)'
  }

  @tuiPure
  getStatus(pkg: PackageDataEntry) {
    return renderPkgStatus(pkg, {})
  }

  get status(): string {
    if (this.pkg.stateInfo.installingInfo) {
      return `Installing...${new InstallingProgressDisplayPipe().transform(this.pkg.stateInfo.installingInfo.progress.overall)}`
    }

    switch (this.getStatus(this.pkg).primary) {
      case PrimaryStatus.Running:
        return 'Running'
      case PrimaryStatus.Stopped:
        return 'Stopped'
      case PrimaryStatus.NeedsConfig:
        return 'Needs Config'
      case PrimaryStatus.Updating:
        return 'Updating...'
      case PrimaryStatus.Stopping:
        return 'Stopping...'
      case PrimaryStatus.Starting:
        return 'Starting...'
      case PrimaryStatus.BackingUp:
        return 'Backing Up...'
      case PrimaryStatus.Restarting:
        return 'Restarting...'
      case PrimaryStatus.Removing:
        return 'Removing...'
      case PrimaryStatus.Restoring:
        return 'Restoring...'
      default:
        return 'Unknown'
    }
  }

  get color(): string {
    if (this.pkg.stateInfo.installingInfo) {
      return 'var(--tui-info-fill)'
    }

    switch (this.getStatus(this.pkg).primary) {
      case PrimaryStatus.Running:
        return 'var(--tui-success-fill)'
      case PrimaryStatus.NeedsConfig:
        return 'var(--tui-warning-fill)'
      case PrimaryStatus.Updating:
      case PrimaryStatus.Stopping:
      case PrimaryStatus.Starting:
      case PrimaryStatus.BackingUp:
      case PrimaryStatus.Restarting:
      case PrimaryStatus.Removing:
      case PrimaryStatus.Restoring:
        return 'var(--tui-info-fill)'
      default:
        return 'var(--tui-text-02)'
    }
  }
}
