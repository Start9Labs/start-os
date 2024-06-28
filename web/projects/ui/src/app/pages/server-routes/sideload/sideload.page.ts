import { Component } from '@angular/core'
import { isPlatform, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import cbor from 'cbor'
import { ErrorToastService } from '@start9labs/shared'
import { S9pk, T } from '@start9labs/start-sdk'

interface Positions {
  [key: string]: [bigint, bigint] // [position, length]
}

const MAGIC = new Uint8Array([59, 59])
const VERSION_1 = new Uint8Array([1])
const VERSION_2 = new Uint8Array([1])

@Component({
  selector: 'sideload',
  templateUrl: './sideload.page.html',
  styleUrls: ['./sideload.page.scss'],
})
export class SideloadPage {
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')
  toUpload: {
    manifest: T.Manifest | null
    icon: string | null
    file: File | null
  } = {
    manifest: null,
    icon: null,
    file: null,
  }
  onTor = this.config.isTor()
  uploadState?: {
    invalid: boolean
    message: string
  }

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
    if (!files || !files.length) return
    const file = files[0]
    if (!file) return
    this.toUpload.file = file
    this.uploadState = await this.validateS9pk(file)
  }

  async validateS9pk(file: File) {
    const magic = new Uint8Array(await blobToBuffer(file.slice(0, 2)))
    const version = new Uint8Array(await blobToBuffer(file.slice(2, 3)))
    if (compare(magic, MAGIC)) {
      try {
        if (compare(version, VERSION_1)) {
          await this.parseS9pkV1(file)
          return {
            invalid: false,
            message: 'A valid package file has been detected!',
          }
        } else if (compare(version, VERSION_2)) {
          await this.parseS9pkV2(file)
          return {
            invalid: false,
            message: 'A valid package file has been detected!',
          }
        } else {
          return {
            invalid: true,
            message: 'Invalid package file',
          }
        }
      } catch (e) {
        return {
          invalid: true,
          message:
            e instanceof Error
              ? `Invalid package file: ${e.message}`
              : 'Invalid package file',
        }
      }
    } else {
      return {
        invalid: true,
        message: 'Invalid package file',
      }
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
      const res = await this.api.sideloadPackage()
      this.api
        .uploadPackage(res.upload, this.toUpload.file!)
        .catch(e => console.error(e))

      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
      this.clearToUpload()
    }
  }

  async parseS9pkV1(file: File) {
    const positions: Positions = {}
    // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103byte is starting point
    let start = 103
    let end = start + 1 // 104
    const tocLength = new DataView(
      await blobToBuffer(file.slice(99, 103) ?? new Blob()),
    ).getUint32(0, false)
    await getPositions(start, end, file, positions, tocLength as any)

    await this.getManifest(positions, file)
    await this.getIcon(positions, file)
  }

  async parseS9pkV2(file: File) {
    const s9pk = await S9pk.deserialize(file, null)
    this.toUpload.manifest = s9pk.manifest
    this.toUpload.icon = await s9pk.icon()
  }

  async getManifest(positions: Positions, file: Blob) {
    const data = await blobToBuffer(
      file.slice(
        Number(positions['manifest'][0]),
        Number(positions['manifest'][0]) + Number(positions['manifest'][1]),
      ),
    )
    this.toUpload.manifest = await cbor.decode(data, true)
  }

  async getIcon(positions: Positions, file: Blob) {
    const contentType = '' // @TODO
    const data = file.slice(
      Number(positions['icon'][0]),
      Number(positions['icon'][0]) + Number(positions['icon'][1]),
      contentType,
    )
    this.toUpload.icon = await blobToDataURL(data)
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
