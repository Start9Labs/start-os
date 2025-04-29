import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import {
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
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
        {{ phase.name }}
        <span>{{ percent }}%</span>
        <progress
          tuiProgressBar
          size="m"
          [style.color]="
            phase.progress === true
              ? 'var(--tui-text-positive)'
              : 'var(--tui-text-action)'
          "
          [value]="percent / 100"
        ></progress>
      </div>
    }
  `,
  styles: `
    div {
      padding: 0.7rem 0;
      span {
        text-align: right;
      }
    }

    :host {
      grid-column: span 6;
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

  async cancel() {
    const loader = this.loader.open('' as i18nKey).subscribe()

    try {
      await this.api.cancelInstallPackage({ id: getManifest(this.pkg).id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
