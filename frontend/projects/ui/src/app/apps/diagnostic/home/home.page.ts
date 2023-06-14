import { Component, Inject } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { LoadingService } from 'src/app/common/loading/loading.service'
import { DiagnosticService } from '../services/diagnostic.service'

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  restarted = false
  error?: {
    code: number
    problem: string
    solution: string
    details?: string
  }

  constructor(
    private readonly loader: LoadingService,
    private readonly api: DiagnosticService,
    private readonly dialogs: TuiDialogService,
    @Inject(WINDOW) private readonly window: Window,
  ) {}

  async ngOnInit() {
    try {
      const error = await this.api.getError()
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
          problem: 'Filesystem I/O error.',
          solution:
            'Repairing the disk could help resolve this issue. Please DO NOT unplug the drive or server during this time or the situation will become worse.',
          details: error.data?.details,
        }
        // disk management error - disk needs repair
      } else if (error.code === 48) {
        this.error = {
          code: 48,
          problem: 'Disk management error.',
          solution:
            'Repairing the disk could help resolve this issue. Please DO NOT unplug the drive or server during this time or the situation will become worse.',
          details: error.data?.details,
        }
      } else {
        this.error = {
          code: error.code,
          problem: error.message,
          solution: 'Please contact support.',
          details: error.data?.details,
        }
      }
    } catch (e) {
      console.error(e)
    }
  }

  async restart(): Promise<void> {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async forgetDrive(): Promise<void> {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.forgetDrive()
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async presentAlertSystemRebuild() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          no: 'Cancel',
          yes: 'Rebuild',
          content:
            '<p>This action will tear down all service containers and rebuild them from scratch. No data will be deleted.</p><p>A system rebuild can be useful if your system gets into a bad state, and it should only be performed if you are experiencing general performance or reliability issues.</p><p>It may take up to an hour to complete. During this time, you will lose all connectivity to your Start9 server.</p>',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        try {
          this.systemRebuild()
        } catch (e) {
          console.error(e)
        }
      })
  }

  async presentAlertRepairDisk() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          no: 'Cancel',
          yes: 'Repair',
          content:
            '<p>This action should only be executed if directed by a Start9 support specialist.</p><p>If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem <i>will</i> be in an unrecoverable state. Please proceed with caution.</p>',
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

  private async systemRebuild(): Promise<void> {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.systemRebuild()
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async repairDisk(): Promise<void> {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.repairDisk()
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
