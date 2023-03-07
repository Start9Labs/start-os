import { Component } from '@angular/core'
import { IonicSlides } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import SwiperCore, { Swiper } from 'swiper'
import { DiskInfo, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'

SwiperCore.use([IonicSlides])

@Component({
  selector: 'app-home',
  templateUrl: 'home.page.html',
  styleUrls: ['home.page.scss'],
})
export class HomePage {
  swiper?: Swiper
  disks: DiskInfo[] = []
  selectedDisk?: DiskInfo
  error = ''

  constructor(
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly dialogs: TuiDialogService,
  ) {}

  async ngOnInit() {
    this.disks = await this.api.getDisks()
  }

  async ionViewDidEnter() {
    if (this.swiper) {
      this.swiper.allowTouchMove = false
    }
  }

  setSwiperInstance(swiper: any) {
    this.swiper = swiper
  }

  next(disk: DiskInfo) {
    this.selectedDisk = disk
    this.swiper?.slideNext(500)
  }

  previous() {
    this.swiper?.slidePrev(500)
  }

  async tryInstall(overwrite: boolean) {
    if (overwrite) {
      return this.presentAlertDanger()
    }

    this.install(false)
  }

  private async install(overwrite: boolean) {
    const loader = this.loader.open('Installing StartOS...').subscribe()

    try {
      await this.api.install({
        logicalname: this.selectedDisk!.logicalname,
        overwrite,
      })
      this.presentAlertReboot()
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.unsubscribe()
    }
  }

  private presentAlertDanger() {
    const { vendor, model } = this.selectedDisk!

    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content: `This action will COMPLETELY erase the disk ${
            vendor || 'Unknown Vendor'
          } - ${model || 'Unknown Model'} and install StartOS in its place`,
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.install(true)
      })
  }

  private async presentAlertReboot() {
    this.dialogs
      .open(
        'Remove the USB stick and reboot your device to begin using your new Start9 server',
        {
          label: 'Install Success',
          closeable: false,
          dismissible: false,
          size: 's',
          data: { button: 'Reboot' },
        },
      )
      .subscribe({
        complete: () => {
          this.reboot()
        },
      })
  }

  private async reboot() {
    const loader = this.loader.open('').subscribe()

    try {
      await this.api.reboot()
      this.presentAlertComplete()
    } catch (e: any) {
      this.error = e.message
    } finally {
      loader.unsubscribe()
    }
  }

  private presentAlertComplete() {
    this.dialogs
      .open('Please wait for StartOS to restart, then refresh this page', {
        label: 'Rebooting',
        size: 's',
      })
      .subscribe()
  }
}
