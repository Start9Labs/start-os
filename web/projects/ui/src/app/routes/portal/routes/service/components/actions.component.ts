import { TuiButton } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { tuiButtonOptionsProvider } from '@taiga-ui/core'
import { DependencyInfo } from 'src/app/routes/portal/routes/service/types/dependency-info'
import { ActionsService } from 'src/app/services/actions.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PackageStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'

const STOPPABLE = ['running', 'starting', 'restarting']

@Component({
  selector: 'service-actions',
  template: `
    @if (canStop) {
      <button
        tuiButton
        appearance="danger-solid"
        iconStart="@tui.square"
        (click)="actions.stop(manifest)"
      >
        Stop
      </button>
    }

    @if (canRestart) {
      <button
        tuiButton
        iconStart="@tui.rotate-cw"
        (click)="actions.restart(manifest)"
      >
        Restart
      </button>
    }

    @if (canStart) {
      <button
        tuiButton
        iconStart="@tui.play"
        (click)="actions.start(manifest, hasUnmet(service.dependencies))"
      >
        Start
      </button>
    }

    @if (canConfigure) {
      <button
        tuiButton
        appearance="secondary-warning"
        iconStart="@tui.wrench"
        (click)="actions.configure(manifest)"
      >
        Configure
      </button>
    }
  `,
  styles: [
    `
      :host {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        padding-bottom: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton],
  providers: [tuiButtonOptionsProvider({ size: 's' })],
})
export class ServiceActionsComponent {
  @Input({ required: true })
  service!: {
    pkg: PackageDataEntry
    dependencies: readonly DependencyInfo[]
    status: PackageStatus
  }

  readonly actions = inject(ActionsService)

  get manifest(): T.Manifest {
    return getManifest(this.service.pkg)
  }

  get canStop(): boolean {
    return STOPPABLE.includes(this.service.status.primary)
  }

  get canStart(): boolean {
    return this.service.status.primary === 'stopped' && !this.canConfigure
  }

  get canRestart(): boolean {
    return this.service.status.primary === 'running'
  }

  get canConfigure(): boolean {
    // @TODO Matt should we just drop this?
    // return !this.service.pkg.status.configured
    return false
  }

  @tuiPure
  hasUnmet(dependencies: readonly DependencyInfo[]): boolean {
    return dependencies.some(dep => !!dep.errorText)
  }
}
