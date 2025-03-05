import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { copyToClipboard } from '@start9labs/shared'
import {
  TuiAlertService,
  TuiButton,
  TuiDialogService,
  TuiIcon,
} from '@taiga-ui/core'
import { TuiLineClamp, TuiTooltip } from '@taiga-ui/kit'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'service-error',
  template: `
    <header>Error</header>
    <tui-line-clamp
      [linesLimit]="2"
      [content]="error?.message"
      (overflownChange)="overflow = $event"
    />
    <h4>
      Actions
      <tui-icon [tuiTooltip]="hint" />
    </h4>
    <ng-template #hint>
      <div>
        <b>Rebuild Container</b>
        is harmless action that and only takes a few seconds to complete. It
        will likely resolve this issue.
      </div>
      <b>Uninstall Service</b>
      is a dangerous action that will remove the service from StartOS and wipe
      all its data.
    </ng-template>
    <p style="display: flex; flex-wrap: wrap; gap: 1rem">
      <button tuiButton (click)="rebuild()">Rebuild Container</button>
      <button tuiButton appearance="negative" (click)="uninstall()">
        Uninstall Service
      </button>
      @if (overflow) {
        <button tuiButton appearance="secondary-grayscale" (click)="show()">
          View full message
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
  imports: [TuiButton, TuiIcon, TuiTooltip, TuiLineClamp],
})
export class ServiceErrorComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly alerts = inject(TuiAlertService)
  private readonly service = inject(StandardActionsService)

  @Input({ required: true })
  pkg!: PackageDataEntry

  overflow = false

  get error() {
    return this.pkg.status.main === 'error' ? this.pkg.status : null
  }

  async copy(text: string): Promise<void> {
    const success = await copyToClipboard(text)

    this.alerts
      .open(success ? 'Copied to clipboard!' : 'Failed to copy to clipboard.', {
        appearance: success ? 'positive' : 'negative',
      })
      .subscribe()
  }

  rebuild() {
    this.service.rebuild(getManifest(this.pkg).id)
  }

  uninstall() {
    this.service.uninstall(getManifest(this.pkg))
  }

  show() {
    this.dialogs
      .open(this.error?.message, { label: 'Service error' })
      .subscribe()
  }
}
