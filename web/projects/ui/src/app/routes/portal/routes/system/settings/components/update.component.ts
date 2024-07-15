import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { CommonModule } from '@angular/common'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { EOSService } from 'src/app/services/eos.service'
import { UPDATE } from '../modals/update.component'

@Component({
  selector: 'settings-update',
  template: `
    <button
      class="g-action"
      [disabled]="service.updatingOrBackingUp$ | async"
      (click)="onClick()"
    >
      <tui-icon icon="@tui.cloud-download"></tui-icon>
      <div tuiTitle>
        <strong>Software Update</strong>
        <div tuiSubtitle>Get the latest version of StartOS</div>
        <div
          *ngIf="updated; else notUpdated"
          tuiSubtitle
          [style.color]="'var(--tui-status-warning)'"
        >
          Update Complete. Restart to apply changes
        </div>
        <ng-template #notUpdated>
          <ng-container *ngIf="service.showUpdate$ | async; else check">
            <div tuiSubtitle [style.color]="'var(--tui-status-positive)'">
              <tui-icon class="small" icon="@tui.zap" />
              Update Available
            </div>
          </ng-container>
          <ng-template #check>
            <div tuiSubtitle [style.color]="'var(--tui-status-info)'">
              <tui-icon class="small" icon="@tui.rotate-cw" />
              Check for updates
            </div>
          </ng-template>
        </ng-template>
      </div>
    </button>
  `,
  styles: `
    :host {
      display: block;
      box-shadow: 0 1px var(--tui-background-neutral-1);
    }

    button {
      cursor: pointer;
    }

    .small {
      font-size: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiIcon, TuiTitle],
})
export class SettingsUpdateComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)

  readonly service = inject(EOSService)

  @Input()
  updated = false

  onClick() {
    this.service.updateAvailable$.value ? this.update() : this.check()
  }

  private update() {
    this.dialogs.open(UPDATE).subscribe()
  }

  private async check(): Promise<void> {
    const loader = this.loader.open('Checking for updates').subscribe()

    try {
      await this.service.loadEos()

      if (this.service.updateAvailable$.value) {
        this.update()
      } else {
        this.showLatest()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private showLatest() {
    this.dialogs
      .open('You are on the latest version of StartOS.', {
        label: 'Up to date!',
        size: 's',
      })
      .subscribe()
  }
}
