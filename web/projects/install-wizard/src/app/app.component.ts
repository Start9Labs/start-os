import { TUI_CONFIRM } from '@taiga-ui/kit'
import { Component, inject } from '@angular/core'
import { DiskInfo, i18nKey, LoadingService, toGuid } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { filter, from } from 'rxjs'
import { SUCCESS, toWarning } from 'src/app/app.utils'
import { ApiService } from 'src/app/services/api.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
  standalone: false,
})
export class AppComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)

  readonly disks$ = from(this.api.getDisks())
  selected: DiskInfo | null = null
  error = ''

  get guid() {
    return toGuid(this.selected)
  }

  async install(overwrite = false) {
    const loader = this.loader.open('Installing StartOS' as i18nKey).subscribe()
    const logicalname = this.selected?.logicalname || ''

    try {
      await this.api.install({ logicalname, overwrite })
      this.reboot()
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.unsubscribe()
    }
  }

  warn() {
    this.dialogs
      .open(TUI_CONFIRM, toWarning(this.selected))
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.install(true)
      })
  }

  private async reboot() {
    this.dialogs
      .open('1. Remove the USB stick<br />2. Click "Reboot" below', SUCCESS)
      .subscribe({
        complete: async () => {
          const loader = this.loader.open().subscribe()

          try {
            await this.api.reboot()
            this.dialogs
              .open(
                'Please wait 1-2 minutes, then refresh this page to access the StartOS setup wizard.',
                {
                  label: 'Rebooting',
                  size: 's',
                  closeable: false,
                  dismissible: false,
                },
              )
              .subscribe()
          } catch (e: any) {
            this.error = e.message
          } finally {
            loader.unsubscribe()
          }
        },
      })
  }
}
