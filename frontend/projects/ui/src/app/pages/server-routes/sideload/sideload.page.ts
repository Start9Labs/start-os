import { Component } from '@angular/core'
import { isPlatform, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Manifest } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'
import cbor from 'cbor'
import { ErrorToastService } from '@start9labs/shared'
interface Positions {
  [key: string]: [bigint, bigint] // [position, length]
}

const MAGIC = new Uint8Array([59, 59])
const VERSION = new Uint8Array([1])
@Component({
  selector: 'sideload',
  templateUrl: './sideload.page.html',
  styleUrls: ['./sideload.page.scss'],
})
export class SideloadPage {
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')
  toUpload: {
    manifest: Manifest | null
    icon: string | null
    file: File | null
  } = {
    manifest: null,
    icon: null,
    file: null,
  }
  onTor = this.config.isTor()
  valid: boolean
  message: string

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly navCtrl: NavController,
    private readonly errToast: ErrorToastService,
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
    const loader = await this.loadingCtrl.create({
      message: 'Verifying package',
      cssClass: 'loader',
    })
    await loader.present()
    if (!files || !files.length) return
    this.toUpload.file = files[0]
    // verify valid s9pk
    const magic = new Uint8Array(
      await readBlobToArrayBuffer(this.toUpload.file.slice(0, 2)),
    )
    const version = new Uint8Array(
      await readBlobToArrayBuffer(this.toUpload.file.slice(2, 3)),
    )
    if (compare(magic, MAGIC) && compare(version, VERSION)) {
      loader.dismiss()
      this.valid = true
      this.message = 'A valid package file has been detected!'
      await this.parseS9pk(this.toUpload.file)
    } else {
      loader.dismiss()
      this.valid = false
      this.message = 'Invalid package file'
    }
  }

  clearToUpload() {
    this.toUpload.file = null
    this.toUpload.manifest = null
    this.toUpload.icon = null
  }

  async handleUpload() {
    const loader = await this.loadingCtrl.create({
      message: 'Uploading package',
      cssClass: 'loader',
    })
    await loader.present()
    try {
      const guid = await this.api.sideloadPackage({
        manifest: this.toUpload.manifest!,
        icon: this.toUpload.icon!,
      })
      this.api
        .uploadPackage(guid, await readBlobToArrayBuffer(this.toUpload.file!))
        .catch(e => {
          this.errToast.present(e)
        })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
      await this.navCtrl.navigateForward(
        `/services/${this.toUpload.manifest!.id}`,
      )
      this.clearToUpload()
    }
  }

  async parseS9pk(file: Blob) {
    const positions: Positions = {}
    // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103byte is starting point
    let start = 103
    let end = start + 1 // 104
    const tocLength = new DataView(
      await readBlobToArrayBuffer(
        this.toUpload.file?.slice(99, 103) ?? new Blob(),
      ),
    ).getUint32(0, false)
    await getPositions(start, end, file, positions, tocLength as any)

    await this.getManifest(positions, file)
    await this.getIcon(positions, file)
  }

  async getManifest(positions: Positions, file: Blob) {
    const data = await readBlobToArrayBuffer(
      file.slice(
        Number(positions['manifest'][0]),
        Number(positions['manifest'][0]) + Number(positions['manifest'][1]),
      ),
    )
    this.toUpload.manifest = await cbor.decode(data, true)
  }

  async getIcon(positions: Positions, file: Blob) {
    const data = file.slice(
      Number(positions['icon'][0]),
      Number(positions['icon'][0]) + Number(positions['icon'][1]),
    )
    this.toUpload.icon = await readBlobAsDataURL(data)
  }
}

async function getPositions(
  initialStart: number,
  initialEnd: number,
  file: Blob,
  positions: Positions,
  tocLength: number,
) {
  let start = initialStart
  let end = initialEnd
  const titleLength = new Uint8Array(
    await readBlobToArrayBuffer(file.slice(start, end)),
  )[0]
  const tocTitle = await file.slice(end, end + titleLength).text()
  start = end + titleLength
  end = start + 8
  const chapterPosition = new DataView(
    await readBlobToArrayBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)
  start = end
  end = start + 8
  const chapterLength = new DataView(
    await readBlobToArrayBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)

  positions[tocTitle] = [chapterPosition, chapterLength]
  start = end
  end = start + 1
  if (end <= tocLength + (initialStart - 1)) {
    await getPositions(start, end, file, positions, tocLength)
  }
}

async function readBlobAsDataURL(f: Blob): Promise<string> {
  const reader = new FileReader()
  reader.readAsDataURL(f)
  return new Promise(resolve => {
    reader.onloadend = () => {
      resolve(reader.result as string)
    }
  })
}

async function readBlobToArrayBuffer(f: Blob): Promise<ArrayBuffer> {
  const reader = new FileReader()
  reader.readAsArrayBuffer(f)
  return new Promise(resolve => {
    reader.onloadend = () => {
      resolve(reader.result as ArrayBuffer)
    }
  })
}

function compare(a: Uint8Array, b: Uint8Array) {
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false
  }
  return true
}
