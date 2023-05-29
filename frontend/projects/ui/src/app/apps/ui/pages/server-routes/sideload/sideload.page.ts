import { Component } from '@angular/core'
import { isPlatform, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Manifest, MarketplacePkg } from '@start9labs/marketplace'
import { ConfigService } from 'src/app/services/config.service'
import { ErrorToastService } from '@start9labs/shared'
import cbor from 'cbor'

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
  pkgData?: {
    pkg: MarketplacePkg
    file: File
  }
  onTor = this.config.isTor()
  invalid = false

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

  clear() {
    this.pkgData = undefined
    this.invalid = false
  }

  async handleUpload() {
    if (!this.pkgData) return
    const loader = await this.loadingCtrl.create({
      message: 'Uploading package',
      cssClass: 'loader',
    })
    await loader.present()

    const { pkg, file } = this.pkgData

    try {
      const guid = await this.api.sideloadPackage({
        manifest: pkg.manifest,
        icon: pkg.icon,
        size: file.size,
      })
      this.api.uploadPackage(guid, file!).catch(e => console.error(e))

      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
      this.clear()
    }
  }

  private async setFile(files?: File[]) {
    if (!files || !files.length) return
    const file = files[0]
    if (!file) return

    await this.validateS9pk(file)
  }

  private async validateS9pk(file: File) {
    const magic = new Uint8Array(await blobToBuffer(file.slice(0, 2)))
    const version = new Uint8Array(await blobToBuffer(file.slice(2, 3)))
    if (compare(magic, MAGIC) && compare(version, VERSION)) {
      this.pkgData = {
        pkg: await this.parseS9pk(file),
        file,
      }
    } else {
      this.invalid = true
    }
  }

  private async parseS9pk(file: File): Promise<MarketplacePkg> {
    const positions: Positions = {}
    // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103byte is starting point
    let start = 103
    let end = start + 1 // 104
    const tocLength = new DataView(
      await blobToBuffer(file.slice(99, 103) ?? new Blob()),
    ).getUint32(0, false)
    await getPositions(start, end, file, positions, tocLength as any)

    const manifest = await this.getAsset(positions, file, 'manifest')
    const [icon] = await Promise.all([
      this.getIcon(positions, file, manifest),
      // this.getAsset(positions, file, 'license'),
      // this.getAsset(positions, file, 'instructions'),
    ])

    return {
      manifest,
      icon,
      license: '',
      instructions: '',
      categories: [],
      versions: [],
      'dependency-metadata': {},
      'published-at': '',
    }
  }

  private async getAsset(
    positions: Positions,
    file: Blob,
    asset: 'manifest' | 'license' | 'instructions',
  ): Promise<any> {
    const data = await blobToBuffer(
      file.slice(
        Number(positions[asset][0]),
        Number(positions[asset][0]) + Number(positions[asset][1]),
      ),
    )
    return cbor.decode(data, true)
  }

  private async getIcon(
    positions: Positions,
    file: Blob,
    manifest: Manifest,
  ): Promise<string> {
    const contentType = `image/${manifest.assets.icon.split('.').pop()}`
    const data = file.slice(
      Number(positions['icon'][0]),
      Number(positions['icon'][0]) + Number(positions['icon'][1]),
      contentType,
    )
    return blobToDataURL(data)
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
    await blobToBuffer(file.slice(start, end)),
  )[0]
  const tocTitle = await file.slice(end, end + titleLength).text()
  start = end + titleLength
  end = start + 8
  const chapterPosition = new DataView(
    await blobToBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)
  start = end
  end = start + 8
  const chapterLength = new DataView(
    await blobToBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)

  positions[tocTitle] = [chapterPosition, chapterLength]
  start = end
  end = start + 1
  if (end <= tocLength + (initialStart - 1)) {
    await getPositions(start, end, file, positions, tocLength)
  }
}

async function readBlobAsDataURL(
  f: Blob | File,
): Promise<string | ArrayBuffer | null> {
  const reader = new FileReader()
  return new Promise((resolve, reject) => {
    reader.onloadend = () => {
      resolve(reader.result)
    }
    reader.readAsDataURL(f)
    reader.onerror = _ => reject(new Error('error reading blob'))
  })
}
async function blobToDataURL(data: Blob | File): Promise<string> {
  const res = await readBlobAsDataURL(data)
  if (res instanceof ArrayBuffer) {
    throw new Error('readBlobAsDataURL response should not be an array buffer')
  }
  if (res == null) {
    throw new Error('readBlobAsDataURL response should not be null')
  }
  if (typeof res === 'string') return res
  throw new Error('no possible blob to data url resolution found')
}

async function blobToBuffer(data: Blob | File): Promise<ArrayBuffer> {
  const res = await readBlobToArrayBuffer(data)
  if (res instanceof String) {
    throw new Error('readBlobToArrayBuffer response should not be a string')
  }
  if (res == null) {
    throw new Error('readBlobToArrayBuffer response should not be null')
  }
  if (res instanceof ArrayBuffer) return res
  throw new Error('no possible blob to array buffer resolution found')
}

async function readBlobToArrayBuffer(
  f: Blob | File,
): Promise<string | ArrayBuffer | null> {
  const reader = new FileReader()
  return new Promise((resolve, reject) => {
    reader.onloadend = () => {
      resolve(reader.result)
    }
    reader.readAsArrayBuffer(f)
    reader.onerror = _ => reject(new Error('error reading blob'))
  })
}

function compare(a: Uint8Array, b: Uint8Array) {
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false
  }
  return true
}
