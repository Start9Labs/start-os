import { Component } from '@angular/core'
import { isPlatform, LoadingController, ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Manifest } from 'src/app/services/patch-db/data-model'
import { BTC_ICON } from 'src/app/services/api/api-icons'
import { ConfigService } from 'src/app/services/config.service'

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
      await parseS9pk(this.toUpload.file)
    } else {
      this.valid = false
      this.message = 'Invalid s9pk detected :('
    }
  }
}

async function readBlobAsText(f: Blob): Promise<string> {
  const reader = new FileReader()
  reader.readAsText(f, 'utf8')
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

async function parseS9pk(s) {
  // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103
  //s.slice(99, 102) = length of toc
  const toc1 = new Uint8Array(await readBlobToArrayBuffer(s.slice(103, 104)))[0]
  console.log('TOC1:' + toc1)
  const toc1_name = await s.slice(104, 104 + toc1).text() // 112
  console.log('TOC1_NAME:', toc1_name)
  const toc1_position = new DataView(
    await readBlobToArrayBuffer(s.slice(104 + toc1, 104 + toc1 + 8)),
  )
  const toc1_pos = toc1_position.getBigUint64(0, false)
  console.log('TOC1_POS:', toc1_pos)
  const toc1_length = new DataView(
    await readBlobToArrayBuffer(s.slice(104 + toc1 + 8, 104 + toc1 + 8 + 8)),
  )
  const toc1_len = toc1_length.getBigUint64(0, false)
  console.log('TOC1_LEN:', toc1_len)
  // TODO convert bigint into regular number
  const m = await readBlobToArrayBuffer(
    s.slice(toc1_pos.toString(10), (toc1_pos + toc1_len).toString(10)),
  )
  const cbor = require('cbor-web')
  cbor.decodeAll(m, (error, obj) => {
    // If there was an error, error != null
    // obj is the unpacked object
    console.log('TOC_ERROR: ' + error)
    console.log('TOC_OBJ: ', obj)
  })
}

function getManifest(): Manifest {
  // get position and length
  // offset from position and read to length

  // const manfiestIndex = f.slice(5, 6)
  // const manfiest = f.slice(7, 14)
  throw new Error('Function not implemented.')
}

function getIcon(): string {
  throw new Error('Function not implemented.')
}
