import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiProgress } from '@taiga-ui/kit'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'service-install-progress',
  template: `
    <header>
      {{ 'Install Progress' | i18n }}
      <button
        tuiButton
        size="xs"
        appearance="primary-destructive"
        [style.margin-inline-start]="'auto'"
        (click)="cancel()"
      >
        {{ 'Cancel' | i18n }}
      </button>
    </header>

    @for (
      phase of pkg.stateInfo.installingInfo?.progress?.phases;
      track $index
    ) {
      <div *tuiLet="phase.progress | installingProgress as percent">
        {{ $any(phase.name) | i18n }}:
        @if (phase.progress === null) {
          <span>{{ 'waiting' | i18n }}</span>
        } @else if (phase.progress === true) {
          <span>{{ 'complete' | i18n }}!</span>
        } @else if (phase.progress === false || phase.progress.total === null) {
          <span>{{ 'in progress' | i18n }}...</span>
        } @else {
          <span>{{ percent }}%</span>
        }
        <progress
          tuiProgressBar
          size="m"
          [max]="100"
          [class.g-positive]="phase.progress === true"
          [value]="isIndeterminate(phase.progress) ? undefined : percent"
        ></progress>
      </div>
    }
  `,
  styles: `
    :host {
      grid-column: span 6;
      color: var(--tui-text-secondary);
    }

    div {
      padding: 0.25rem 0;
    }

    span {
      float: right;
      text-transform: capitalize;
    }

    progress {
      margin: 0.5rem 0;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiProgress, TuiLet, InstallingProgressPipe, i18nPipe, TuiButton],
})
export class ServiceInstallProgressComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)

  isIndeterminate(progress: T.Progress): boolean {
    return (
      progress === false ||
      (!!progress && progress !== true && progress.total === null)
    )
  }

  async cancel() {
    const loader = this.loader.open().subscribe()

    try {
      await this.api.cancelInstallPackage({ id: getManifest(this.pkg).id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
