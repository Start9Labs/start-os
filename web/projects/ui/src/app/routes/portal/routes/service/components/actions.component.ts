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
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'service-actions',
  template: `
    @if (pkg.status.main.status === 'running') {
      <button
        tuiButton
        appearance="danger-solid"
        iconStart="@tui.square"
        (click)="actions.stop(manifest)"
      >
        Stop
      </button>

      <button
        tuiButton
        iconStart="@tui.rotate-cw"
        (click)="actions.restart(manifest)"
      >
        Restart
      </button>
    }

    @if (pkg.status.main.status === 'stopped' && isConfigured) {
      <button
        tuiButton
        iconStart="@tui.play"
        (click)="actions.start(manifest, hasUnmet(dependencies))"
      >
        Start
      </button>
    }

    @if (!isConfigured) {
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
  pkg!: PackageDataEntry

  @Input({ required: true })
  dependencies: readonly DependencyInfo[] = []

  readonly actions = inject(ActionsService)

  get isConfigured(): boolean {
    return this.pkg.status.configured
  }

  get manifest(): T.Manifest {
    return getManifest(this.pkg)
  }

  @tuiPure
  hasUnmet(dependencies: readonly DependencyInfo[]): boolean {
    return dependencies.some(dep => !!dep.errorText)
  }
}