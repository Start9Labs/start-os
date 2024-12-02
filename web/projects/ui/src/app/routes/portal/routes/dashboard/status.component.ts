import { TuiLoader, TuiIcon } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { tuiPure } from '@taiga-ui/cdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { InstallingProgressDisplayPipe } from '../service/pipes/install-progress.pipe'

@Component({
  standalone: true,
  selector: 'td[appStatus]',
  template: `
    @if (loading) {
      <tui-loader size="s" />
    } @else {
      @if (healthy) {
        <tui-icon icon="@tui.check" class="g-success" />
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
  providers: [InstallingProgressDisplayPipe],
})
export class StatusComponent {
  private readonly pipe = inject(InstallingProgressDisplayPipe)

  @Input()
  pkg!: PackageDataEntry

  @Input()
  hasDepErrors = false

  get healthy(): boolean {
    const status = this.getStatus(this.pkg)

    return (
      !this.hasDepErrors && // no deps error
      // @TODO Matt how do we handle this now?
      // !!this.pkg.status.configured && // no config needed
      status.health !== 'failure' // no health issues
    )
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
      return `Installing...${this.pipe.transform(this.pkg.stateInfo.installingInfo.progress.overall)}`
    }

    switch (this.getStatus(this.pkg).primary) {
      case 'running':
        return 'Running'
      case 'stopped':
        return 'Stopped'
      // @TODO Matt just dropping this?
      // case 'needsConfig':
      //   return 'Needs Config'
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
      // @TODO Matt just dropping this?
      // case 'needsConfig':
      //   return 'var(--tui-status-warning)'
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
