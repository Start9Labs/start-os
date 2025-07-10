import { Component, Inject } from '@angular/core'
import { WA_WINDOW } from '@ng-web-apis/common'
import { DialogService, i18nKey, LoadingService } from '@start9labs/shared'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'diagnostic-home',
  templateUrl: 'home.component.html',
  styleUrls: ['home.page.scss'],
  standalone: false,
})
export class HomePage {
  restarted = false
  error?: {
    code: number
    problem: i18nKey
    solution: i18nKey
    details?: string
  }

  constructor(
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly dialog: DialogService,
    @Inject(WA_WINDOW) private readonly window: Window,
    readonly config: ConfigService,
  ) {}

  async ngOnInit() {
    try {
      const error = await this.api.diagnosticGetError()
      // incorrect drive
      if (error.code === 15) {
        this.error = {
          code: 15,
          problem: 'Unknown storage drive detected',
          solution:
            'To use a different storage drive, replace the current one and click RESTART SERVER below. To use the current storage drive, click USE CURRENT DRIVE below, then follow instructions. No data will be erased during this process.',
          details: error.data?.details,
        }
        // no drive
      } else if (error.code === 20) {
        this.error = {
          code: 20,
          problem: 'Storage drive not found',
          solution:
            'Insert your StartOS storage drive and click RESTART SERVER below.',
          details: error.data?.details,
        }
        // drive corrupted
      } else if (error.code === 25) {
        this.error = {
          code: 25,
          problem:
            'Storage drive corrupted. This could be the result of data corruption or physical damage.',
          solution:
            'It may or may not be possible to re-use this drive by reformatting and recovering from backup. To enter recovery mode, click ENTER RECOVERY MODE below, then follow instructions. No data will be erased during this step.',
          details: error.data?.details,
        }
        // filesystem I/O error - disk needs repair
      } else if (error.code === 2) {
        this.error = {
          code: 2,
          problem: 'Filesystem error',
          solution:
            'Repairing the disk could help resolve this issue. Please DO NOT unplug the drive or server during this time or the situation will become worse.',
          details: error.data?.details,
        }
        // disk management error - disk needs repair
      } else if (error.code === 48) {
        this.error = {
          code: 48,
          problem: 'Disk management error',
          solution:
            'Repairing the disk could help resolve this issue. Please DO NOT unplug the drive or server during this time or the situation will become worse.',
          details: error.data?.details,
        }
      } else {
        this.error = {
          code: error.code,
          problem: error.message as i18nKey,
          solution: 'Please contact support',
          details: error.data?.details,
        }
      }
    } catch (e) {
      console.error(e)
    }
  }

  async restart(): Promise<void> {
    const loader = this.loader.open('Loading').subscribe()

    try {
      await this.api.diagnosticRestart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async forgetDrive(): Promise<void> {
    const loader = this.loader.open('Loading').subscribe()

    try {
      await this.api.diagnosticForgetDrive()
      await this.api.diagnosticRestart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async presentAlertRepairDisk() {
    this.dialog
      .openConfirm({
        label: 'Warning',
        size: 's',
        data: {
          no: 'Cancel',
          yes: 'Repair',
          content:
            'This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action. If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem will be in an unrecoverable state. Please proceed with caution.',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        try {
          this.repairDisk()
        } catch (e) {
          console.error(e)
        }
      })
  }

  refreshPage(): void {
    this.window.location.reload()
  }

  private async repairDisk(): Promise<void> {
    const loader = this.loader.open('Loading').subscribe()

    try {
      await this.api.diagnosticRepairDisk()
      await this.api.diagnosticRestart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
