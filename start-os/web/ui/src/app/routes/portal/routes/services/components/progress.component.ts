import { Component, inject, Input } from '@angular/core'
import {
  ErrorService,
  i18nPipe,
  leafProgress,
  LeafProgressPipe,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiExpand } from '@taiga-ui/core'
import { TuiNotificationMiddleService, TuiProgress } from '@taiga-ui/kit'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { InstallPhaseViewPipe } from 'src/app/routes/portal/routes/services/pipes/install-phase-view.pipe'
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
      @let view = phase.progress | installPhaseView;
      @let leaf = view.progress | leafProgress;
      @let percent = leaf | installingProgress;
      <div>
        <b>{{ $any(phase.name) | i18n }}</b>
        @if (leaf === null) {
          <span>{{ 'waiting' | i18n }}</span>
        } @else if (leaf === true) {
          <span>{{ 'complete' | i18n }}!</span>
        } @else if (leaf === false || leaf.total === null) {
          <span>{{ 'in progress' | i18n }}...</span>
        } @else {
          <span>{{ percent }}%</span>
        }
        <tui-expand [expanded]="!!view.subtext">
          <div class="subtext">{{ $any(view.subtext) | i18n }}</div>
        </tui-expand>
        <progress
          tuiProgressBar
          size="m"
          [max]="100"
          [class.g-positive]="leaf === true"
          [attr.value]="isIndeterminate(leaf) ? undefined : percent"
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

    b {
      color: var(--tui-text-primary);
    }

    .subtext {
      padding: 0;
      font-size: 0.875rem;
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
  imports: [
    TuiProgress,
    TuiExpand,
    InstallingProgressPipe,
    InstallPhaseViewPipe,
    LeafProgressPipe,
    i18nPipe,
    TuiButton,
  ],
})
export class ServiceInstallProgressComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(TuiNotificationMiddleService)

  isIndeterminate(progress: T.Progress): boolean {
    const leaf = leafProgress(progress)
    return leaf === false || (!!leaf && leaf !== true && leaf.total === null)
  }

  async cancel() {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.cancelInstallPackage({ id: getManifest(this.pkg).id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
