import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiLineClamp, TuiTooltip } from '@taiga-ui/kit'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'service-error',
  template: `
    <header>{{ 'Error' | i18n }}</header>
    <tui-line-clamp
      [linesLimit]="2"
      [content]="error?.message"
      (overflownChange)="overflow = $event"
    />
    <h4>
      {{ 'Actions' | i18n }}
      <tui-icon [tuiTooltip]="hint" />
    </h4>
    <ng-template #hint>
      <p>
        {{
          '"Rebuild container" is a harmless action that and only takes a few seconds to complete. It will likely resolve this issue.'
            | i18n
        }}
      </p>
      <p>
        {{
          '"Soft uninstall" will remove the service from StartOS but preserve its data.'
            | i18n
        }}
      </p>
      <p>
        {{
          '"Hard uninstall" is a dangerous action that will remove the service from StartOS and wipe all its data.'
            | i18n
        }}
      </p>
    </ng-template>
    <p style="display: flex; flex-wrap: wrap; gap: 1rem">
      <button tuiButton (click)="rebuild()">
        {{ 'Rebuild container' | i18n }}
      </button>
      <button tuiButton appearance="warning" (click)="uninstall()">
        {{ 'Soft uninstall' | i18n }}
      </button>
      <button tuiButton appearance="negative" (click)="uninstall(false)">
        {{ 'Hard uninstall' | i18n }}
      </button>
      @if (overflow) {
        <button tuiButton appearance="secondary-grayscale" (click)="show()">
          {{ 'View full message' | i18n }}
        </button>
      }
    </p>
  `,
  styles: `
    :host {
      grid-column: span 4;
    }

    header {
      --tui-background-neutral-1: var(--tui-status-negative-pale);
    }

    tui-line-clamp {
      pointer-events: none;
      margin: 1rem 0;
      color: var(--tui-status-negative);
    }

    h4 {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      font: var(--tui-font-text-m);
      font-weight: bold;
      color: var(--tui-text-secondary);
      text-transform: uppercase;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiIcon, TuiTooltip, TuiLineClamp, i18nPipe],
})
export class ServiceErrorComponent {
  private readonly dialog = inject(DialogService)
  private readonly service = inject(StandardActionsService)

  @Input({ required: true })
  pkg!: PackageDataEntry

  overflow = false

  get error() {
    return this.pkg.status.main === 'error' ? this.pkg.status : null
  }

  rebuild() {
    this.service.rebuild(getManifest(this.pkg).id)
  }

  uninstall(soft = true) {
    this.service.uninstall(getManifest(this.pkg), { force: true, soft })
  }

  show() {
    this.dialog
      .openAlert(this.error?.message as i18nKey, { label: 'Service error' })
      .subscribe()
  }
}
