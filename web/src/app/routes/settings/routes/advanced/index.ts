import { DOCUMENT } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton } from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  template: `
    <section class="g-form" tuiCardLarge [style.align-items]="'start'">
      <a tuiButton [href]="luciUrl" target="_blank">Launch LuCI Interface</a>
      <button tuiButton>Download Support Diagnostics</button>
      <button tuiButton (click)="factoryReset()">Factory Reset</button>
    </section>
  `,
  host: { class: 'g-page' },
  imports: [TuiCardLarge, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Advanced {
  private readonly api = inject(ApiService)
  private readonly actions = inject(ActionService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly luciUrl = `${inject(DOCUMENT).location.origin.replace(/:(\d+)$/, '')}:8080`

  protected factoryReset() {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Factory Reset',
        data: {
          content:
            'All settings will be erased and the device will reboot. Your WiFi password will be preserved.',
          yes: 'Factory Reset',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.actions.run(() => this.api.systemFactoryReset(), {
          loading: 'Resetting device...',
          success: 'Factory reset initiated. Device is rebooting...',
        })
      })
  }
}
