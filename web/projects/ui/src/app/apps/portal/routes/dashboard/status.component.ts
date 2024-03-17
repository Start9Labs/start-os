import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import {
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { packageLoadingProgress } from 'src/app/util/package-loading-progress'

const LOADING: any[] = [
  PackageMainStatus.BackingUp,
  PackageMainStatus.Starting,
  PackageMainStatus.Stopping,
  PackageMainStatus.Restarting,
]

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
    <b [style.color]="textColor">{{ status }}</b>
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
  appStatus!: PackageDataEntry

  get healthy(): boolean {
    return !!this.appStatus.installed?.status.configured
  }

  get loading(): boolean {
    return (
      !!this.appStatus['install-progress'] ||
      LOADING.includes(this.appStatus.installed?.status.main.status)
    )
  }

  get status(): string {
    // TODO: updating, removing, restoring?
    if (this.appStatus['install-progress']) {
      return `Installing... ${packageLoadingProgress(this.appStatus['install-progress'])?.totalProgress || 0}%`
    }

    switch (this.appStatus.installed?.status.main.status) {
      case PackageMainStatus.Running:
        return 'Running'
      case PackageMainStatus.Stopped:
        return 'Stopped'
      case PackageMainStatus.Starting:
        return 'Starting...'
      case PackageMainStatus.Stopping:
        return 'Stopping...'
      case PackageMainStatus.BackingUp:
        return 'Backing Up...'
      case PackageMainStatus.Restarting:
        return 'Restarting...'
      default:
        return !this.appStatus.installed?.status.configured
          ? 'Needs Config'
          : 'Unknown'
    }
  }

  get textColor(): string {
    if (this.appStatus['install-progress']) {
      return 'var(--tui-info-fill)'
    }

    switch (this.status) {
      case 'Running':
        return 'var(--tui-success-fill)'
      case 'Needs Config':
        return 'var(--tui-warning-fill)'
      case 'Starting...':
      case 'Stopping...':
      case 'Backing Up...':
      case 'Restarting...':
        return 'var(--tui-info-fill)'
      default:
        return 'var(--tui-text-02)'
    }
  }
}
