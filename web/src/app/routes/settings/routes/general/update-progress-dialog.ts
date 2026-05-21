import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
  OnInit,
} from '@angular/core'
import {
  TuiDialogContext,
  TuiLoader,
  TuiNotificationService,
} from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { SystemService } from 'src/app/services/system.service'

/**
 * Blocking progress dialog for a system update. Owns the update lifecycle:
 * kicks off `SystemService.startUpdate` on open, shows a spinner through the
 * update and reboot, and closes itself once the device reconnects (or the
 * update fails to start). Mirrors the RECONNECTING_DIALOG convention used for
 * other network-disrupting actions.
 */
@Component({
  template: `
    <tui-loader size="l" />
    @if (system.rebooting()) {
      Device is restarting&hellip;
      <small class="g-secondary">
        Your network will be briefly unavailable.
      </small>
    } @else {
      Updating to v{{ context.data }}&hellip;
    }
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 0.5rem;
      padding: 1rem 0;
      text-align: center;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class UpdateProgressDialog implements OnInit {
  protected readonly system = inject(SystemService)
  private readonly alerts = inject(TuiNotificationService)
  protected readonly context = injectContext<TuiDialogContext<void, string>>()

  // Latches once the update is underway, so the completion effect does not
  // fire on the initial idle state before `startUpdate` flips `updating`.
  private active = false

  constructor() {
    effect(() => {
      const busy = this.system.updating() || this.system.rebooting()
      if (busy) {
        this.active = true
      } else if (this.active) {
        this.context.completeWith()
      }
    })
  }

  ngOnInit(): void {
    this.system.startUpdate(this.context.data).catch((e: any) => {
      this.alerts
        .open(e?.message || 'Failed to start update', {
          appearance: 'negative',
        })
        .subscribe()
    })
  }
}

export const UPDATE_PROGRESS_DIALOG = new PolymorpheusComponent(
  UpdateProgressDialog,
)
