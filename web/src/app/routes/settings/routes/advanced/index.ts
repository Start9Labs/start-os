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
      <button tuiButton (click)="downloadDiagnostics()">
        Download Support Diagnostics
      </button>
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
  protected readonly luciUrl = '/cgi-bin/luci'

  protected downloadDiagnostics() {
    this.actions.run(
      async () => {
        const res = await fetch('/api/diagnostics', { credentials: 'include' })
        if (!res.ok) throw new Error(`Download failed: ${res.statusText}`)
        const blob = await res.blob()
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        const disposition = res.headers.get('content-disposition')
        const match = disposition?.match(/filename="(.+)"/)
        a.download = match?.[1] ?? 'diagnostics.tar.gz'
        a.click()
        URL.revokeObjectURL(url)
      },
      {
        loading: 'Creating diagnostics bundle...',
        success: 'Diagnostics downloaded',
      },
    )
  }

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
        this.actions.run(
          async () => {
            await this.api.systemFactoryReset()
            // Device is rebooting — poll until it goes down
            while (true) {
              await this.api.systemInfo()
            }
          },
          {
            loading: 'Resetting device...',
            success: 'Factory reset complete',
            restart: true,
          },
        )
      })
  }
}
