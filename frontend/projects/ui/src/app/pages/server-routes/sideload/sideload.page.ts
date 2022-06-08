import { Component } from '@angular/core'
import { isPlatform, LoadingController, ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Manifest } from 'src/app/services/patch-db/data-model'
import { BTC_ICON } from 'src/app/services/api/api-icons'
import { ConfigService } from 'src/app/services/config.service'
import * as cbor from 'cbor-web'
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
    manifest: Manifest
    icon: string
    file: File
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
    // verify valid s9pk
    const magic = await readBlobToArrayBuffer(this.toUpload.file.slice(0, 4))
    const version = await readBlobToArrayBuffer(this.toUpload.file.slice(2, 3))
    if (compare(magic, MAGIC) && compare(version, VERSION)) {
      this.valid = true
      this.message = 'A valid s9pk has been detected!'
      await this.parseS9pk(this.toUpload.file)
    } else {
      this.valid = false
      this.message = 'Invalid s9pk detected :('
    }
  }

  clearToUpload() {
    this.toUpload.file = null
    this.toUpload.manifest = null
    this.toUpload.icon = null
  }

  async handleUpload() {
    console.log('uploading')
  }

  async parseS9pk(file: Blob) {
    const positions: Positions = {}
    // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103byte is starting point
    let start = 103
    let end = start + 1 // 104
    await getPositions(start, end, file, positions)
    console.log('POSITIONS', positions)
    const tocLength = new Uint8Array(
      await readBlobToArrayBuffer(this.toUpload.file.slice(98, 102)),
    )[0]
    console.log('TOC_LEN:', tocLength)

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
    const decoded = await cbor.decodeAll(data, (error, obj) => {
      console.log('OBJ: ', obj)
      if (error) throw new Error(error)
      return obj[0]
    })
    this.toUpload.manifest = decoded[0]
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
) {
  let start = initialStart
  let end = initialEnd
  const titleLength = new Uint8Array(
    await readBlobToArrayBuffer(file.slice(start, end)),
  )[0]
  console.log('TITLE_LENGTH:', titleLength)
  const tocTitle = await file.slice(end, end + titleLength).text()
  console.log('TOC_TITLE:', tocTitle)

  start = end + titleLength
  end = start + 8
  const chapterPosition = new DataView(
    await readBlobToArrayBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)
  console.log('CHAPTER_POSITION:', chapterPosition)
  start = end
  end = start + 8
  const chapterLength = new DataView(
    await readBlobToArrayBuffer(file.slice(start, end)),
  ).getBigUint64(0, false)
  console.log('CHAPTER_LENGTH:', chapterLength)

  positions[tocTitle] = [chapterPosition, chapterLength]
  start = end
  end = start + 1
  // 254 is length of table of contents
  if (end <= 254) {
    await getPositions(start, end, file, positions)
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

function compare(a, b) {
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false
  }
  return true
}
