import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceProgressPhaseComponent } from './progress-phase.component'

@Component({
  selector: 'service-install-progress',
  changeDetection: ChangeDetectionStrategy.OnPush,
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
      phase of pkg().stateInfo.installingInfo?.progress?.phases;
      track phase.name
    ) {
      <service-progress-phase [name]="phase.name" [progress]="phase.progress" />
    }
  `,
  styles: `
    :host {
      grid-column: span 6;
      color: var(--tui-text-secondary);
    }
  `,
  host: { class: 'g-card' },
  imports: [ServiceProgressPhaseComponent, i18nPipe, TuiButton],
})
export class ServiceInstallProgressComponent {
  readonly pkg = input.required<PackageDataEntry>()

  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(TuiNotificationMiddleService)

  async cancel() {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.cancelInstallPackage({ id: getManifest(this.pkg()).id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
