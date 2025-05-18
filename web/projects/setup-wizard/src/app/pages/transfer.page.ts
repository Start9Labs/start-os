import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  DiskInfo,
  DriveComponent,
  ErrorService,
  toGuid,
} from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogOptions,
  TuiDialogService,
  TuiLoader,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <section tuiCardLarge>
      <header>Transfer</header>
      Select the physical drive containing your StartOS data
      @if (loading) {
        <tui-loader />
      }
      @for (drive of drives; track drive) {
        <button tuiCell [drive]="drive" (click)="select(drive)"></button>
      }
      <button tuiButton iconStart="@tui.rotate-cw" (click)="refresh()">
        Refresh
      </button>
    </section>
  `,
  imports: [TuiCardLarge, TuiCell, TuiButton, TuiLoader, DriveComponent],
})
export default class TransferPage {
  private readonly apiService = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  loading = true
  drives: DiskInfo[] = []

  async ngOnInit() {
    this.stateService.setupType = 'transfer'
    await this.getDrives()
  }

  async refresh() {
    await this.getDrives()
  }

  async getDrives() {
    this.loading = true

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

  select(drive: DiskInfo) {
    this.dialogs
      .open(TUI_CONFIRM, OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.stateService.recoverySource = {
          type: 'migrate',
          guid: toGuid(drive) || '',
        }
        this.router.navigate([`storage`])
      })
  }
}

const OPTIONS: Partial<TuiDialogOptions<TuiConfirmData>> = {
  label: 'Warning',
  size: 's',
  data: {
    content:
      'After transferring data from this drive, <b>do not</b> attempt to boot into it again as a Start9 Server. This may result in services malfunctioning, data corruption, or loss of funds.',
    yes: 'Continue',
    no: 'Cancel',
  },
}
