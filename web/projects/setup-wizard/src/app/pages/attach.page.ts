import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  DiskInfo,
  DriveComponent,
  ErrorService,
  i18nKey,
  LoadingService,
  toGuid,
} from '@start9labs/shared'
import { TuiButton, TuiDialogService, TuiLoader } from '@taiga-ui/core'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { PASSWORD } from 'src/app/components/password.component'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header>Use existing drive</header>
      <div>Select the physical drive containing your StartOS data</div>

      @if (loading) {
        <tui-loader />
      } @else {
        @for (drive of drives; track drive) {
          <button tuiCell [drive]="drive" (click)="select(drive)"></button>
        } @empty {
          No valid StartOS data drives found. Please make sure the drive is a
          valid StartOS data drive (not a backup) and is firmly connected, then
          refresh the page.
        }
        <footer>
          <button tuiButton iconStart="@tui.rotate-cw" (click)="refresh()">
            Refresh
          </button>
        </footer>
      }
    </section>
  `,
  imports: [TuiButton, TuiCardLarge, TuiCell, TuiLoader, DriveComponent],
})
export default class AttachPage {
  private readonly apiService = inject(ApiService)
  private readonly router = inject(Router)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)

  loading = true
  drives: DiskInfo[] = []

  async ngOnInit() {
    this.stateService.setupType = 'attach'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      this.drives = await this.apiService
        .getDrives()
        .then(drives => drives.filter(toGuid))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  select(disk: DiskInfo) {
    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Set Password',
        size: 's',
        data: { storageDrive: true },
      })
      .subscribe(password => {
        this.attachDrive(toGuid(disk) || '', password)
      })
  }

  private async attachDrive(guid: string, password: string) {
    const loader = this.loader
      .open('Connecting to drive' as i18nKey)
      .subscribe()

    try {
      await this.stateService.importDrive(guid, password)
      await this.router.navigate([`loading`])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
