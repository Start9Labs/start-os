import { Component } from '@angular/core'
import { AlertController, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  error: {
    code: number
    problem: string
    solution: string
    details?: string
  } = {} as any
  solutions: string[] = []
  restarted = false

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
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
            'To use a different storage drive, replace the current one and click RESTART EMBASSY below. To use the current storage drive, click USE CURRENT DRIVE below, then follow instructions. No data will be erased during this process.',
          details: error.data?.details,
        }
        // no drive
      } else if (error.code === 20) {
        this.error = {
          code: 20,
          problem: 'Storage drive not found',
          solution:
            'Insert your embassyOS storage drive and click RESTART EMBASSY below.',
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
            'Repairing the disk could help resolve this issue. This will occur on a restart between the bep and chime. Please DO NOT unplug the drive or Embassy during this time or the situation will become worse.',
          details: error.data?.details,
        }
        // disk management error - disk needs repair
      } else if (error.code === 48) {
        this.error = {
          code: 48,
          problem: 'Disk management error.',
          solution:
            'Repairing the disk could help resolve this issue. This will occur on a restart between the bep and chime. Please DO NOT unplug the drive or Embassy during this time or the situation will become worse.',
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
    const loader = await this.loadingCtrl.create({
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.dismiss()
    }
  }

  async forgetDrive(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.forgetDrive()
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.dismiss()
    }
  }

  async repairDrive(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.repairDisk()
      await this.api.restart()
      this.restarted = true
    } catch (e) {
      console.error(e)
    } finally {
      loader.dismiss()
    }
  }

  async presentAlertRepairDisk() {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message:
        'This action will attempt to preform a disk repair operation and system reboot. No data will be deleted. This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action. If anything happens to the device during the reboot (between the bep and chime), such as loosing power, a power surge, unplugging the drive, or unplugging the Embassy, the filesystem *will* be in an unrecoverable state. Please proceed with caution.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Repair',
          handler: () => {
            try {
              this.api.repairDisk().then(_ => {
                this.restart()
              })
            } catch (e) {
              console.error(e)
            }
          },
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  refreshPage(): void {
    window.location.reload()
  }
}
