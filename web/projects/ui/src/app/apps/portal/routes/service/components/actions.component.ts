import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { DependencyInfo } from 'src/app/apps/portal/routes/service/types/dependency-info'
import { ActionsService } from 'src/app/apps/portal/services/actions.service'
import {
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'service-actions',
  template: `
    @if (isRunning) {
      <button
        tuiButton
        appearance="secondary-destructive"
        iconLeft="tuiIconSquare"
        (click)="actions.stop(service)"
      >
        Stop
      </button>

      <button
        tuiButton
        appearance="secondary"
        iconLeft="tuiIconRotateCw"
        (click)="actions.restart(service)"
      >
        Restart
      </button>
    }

    @if (isStopped && isConfigured) {
      <button
        tuiButton
        iconLeft="tuiIconPlay"
        (click)="actions.start(service, hasUnmet(dependencies))"
      >
        Start
      </button>
    }

    @if (!isConfigured) {
      <button
        tuiButton
        appearance="secondary-warning"
        iconLeft="tuiIconTool"
        (click)="actions.configure(service)"
      >
        Configure
      </button>
    }
  `,
  styles: [':host { display: flex; gap: 1rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule],
})
export class ServiceActionsComponent {
  @Input({ required: true })
  service!: PackageDataEntry

  @Input({ required: true })
  dependencies: readonly DependencyInfo[] = []

  readonly actions = inject(ActionsService)

  get isConfigured(): boolean {
    return this.service.installed!.status.configured
  }

  get isRunning(): boolean {
    return (
      this.service.installed?.status.main.status === PackageMainStatus.Running
    )
  }

  get isStopped(): boolean {
    return (
      this.service.installed?.status.main.status === PackageMainStatus.Stopped
    )
  }

  @tuiPure
  hasUnmet(dependencies: readonly DependencyInfo[]): boolean {
    return dependencies.some(dep => !!dep.errorText)
  }
}
