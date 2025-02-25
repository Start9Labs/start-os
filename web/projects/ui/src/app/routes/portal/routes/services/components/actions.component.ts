import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { DependencyInfo } from 'src/app/routes/portal/routes/services/types/dependency-info'
import { ControlsService } from 'src/app/services/controls.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'service-actions',
  template: `
    @if (['running', 'starting', 'restarting'].includes(status)) {
      <button
        tuiButton
        appearance="outline-destructive"
        iconStart="@tui.square"
        (click)="actions.stop(manifest)"
      >
        Stop
      </button>
    }

    @if (status === 'running') {
      <button
        tuiButton
        appearance="outline"
        iconStart="@tui.rotate-cw"
        (click)="actions.restart(manifest)"
      >
        Restart
      </button>
    }

    @if (status === 'stopped') {
      <button
        tuiButton
        appearance="outline"
        iconStart="@tui.play"
        (click)="actions.start(manifest, hasUnmet(dependencies))"
      >
        Start
      </button>
    }
  `,
  styles: [
    `
      :host {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(7rem, 1fr));
        gap: 1rem;
        justify-content: center;
        margin-block-start: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton],
})
export class ServiceActionsComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  status!: PrimaryStatus

  // TODO
  dependencies: readonly DependencyInfo[] = []

  readonly actions = inject(ControlsService)

  get manifest(): T.Manifest {
    return getManifest(this.pkg)
  }

  @tuiPure
  hasUnmet(dependencies: readonly DependencyInfo[]): boolean {
    return dependencies.some(dep => !!dep.errorText)
  }
}
