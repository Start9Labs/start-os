import { Component } from '@angular/core'
import { isPlatform, LoadingController, ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Manifest } from 'src/app/services/patch-db/data-model'
import { BTC_ICON } from 'src/app/services/api/api-icons'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'sideload',
  templateUrl: './sideload.page.html',
  styleUrls: ['./sideload.page.scss'],
})
export class SideloadPage {
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')
  toUpload: {
    manifest: Manifest
    icon: string
    file: File
  }
  onTor = this.config.isTor()

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
  ) {}

  handleFileDrop(e: any) {
    const files = e.dataTransfer.files
    this.setFile(files)
  }

  handleFileInput(e: any) {
    const files = e.target.files
    this.setFile(files)
  }

  async setFile(files?: File[]) {
    if (!files || !files.length) return
    this.toUpload.file = files[0]
    // TODO parse s9pk to get manifest, icon
  }

  async fileToArrayBuffer(f: File): Promise<ArrayBuffer> {
    const reader = new FileReader()
    reader.readAsArrayBuffer(f)
    return new Promise(resolve => {
      reader.onloadend = () => {
        resolve(reader.result as ArrayBuffer)
      }
    })
  }
}
