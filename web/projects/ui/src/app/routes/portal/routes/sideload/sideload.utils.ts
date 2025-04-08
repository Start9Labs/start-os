import { MarketplacePkgBase } from '@start9labs/marketplace'
import { S9pk, ExtendedVersion } from '@start9labs/start-sdk'

const MAGIC = new Uint8Array([59, 59])
const VERSION_1 = new Uint8Array([1])
const VERSION_2 = new Uint8Array([2])

export async function validateS9pk(
  file: File,
): Promise<MarketplacePkgBase | string> {
  const magic = new Uint8Array(await blobToBuffer(file.slice(0, 2)))
  const version = new Uint8Array(await blobToBuffer(file.slice(2, 3)))

  if (compare(magic, MAGIC)) {
    try {
      if (compare(version, VERSION_1)) {
        return 'Version 1 s9pk detected. This package format is deprecated. You can sideload a V1 s9pk via start-cli if necessary.'
      } else if (compare(version, VERSION_2)) {
        return await parseS9pk(file)
      } else {
        console.error(version)

        return 'Invalid package file'
      }
    } catch (e) {
      console.error(e)

      return e instanceof Error
        ? `Invalid package file: ${e.message}`
        : 'Invalid package file'
    }
  }

  return 'Invalid package file'
}

async function parseS9pk(file: File): Promise<MarketplacePkgBase> {
  const s9pk = await S9pk.deserialize(file, null)

  return {
    ...s9pk.manifest,
    // dependencyMetadata: await s9pk.getDependencyMetadata(),
    dependencyMetadata: {} as any,
    gitHash: '',
    icon: await s9pk.icon(),
    sourceVersion: s9pk.manifest.canMigrateFrom,
    flavor: ExtendedVersion.parse(s9pk.manifest.version).flavor,
  }
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
  return a.length === b.length && a.every((value, index) => value === b[index])
}
