import { S9pk, T } from '@start9labs/start-sdk'
import cbor from 'cbor'

const MAGIC = new Uint8Array([59, 59])
const VERSION_1 = new Uint8Array([1])
const VERSION_2 = new Uint8Array([2])

interface Positions {
  [key: string]: [bigint, bigint] // [position, length]
}

export async function parseS9pk(
  file: File,
): Promise<(T.Manifest & { icon: string }) | string> {
  const magic = new Uint8Array(await blobToBuffer(file.slice(0, 2)))
  const version = new Uint8Array(await blobToBuffer(file.slice(2, 3)))

  if (compare(magic, MAGIC)) {
    try {
      if (compare(version, VERSION_1)) {
        return await parseS9pkV1(file)
      } else if (compare(version, VERSION_2)) {
        const s9pk = await S9pk.deserialize(file, null)

        return {
          ...s9pk.manifest,
          icon: await s9pk.icon(),
        }
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

async function parseS9pkV1(file: File) {
  const positions: Positions = {}
  // magic=2bytes, version=1bytes, pubkey=32bytes, signature=64bytes, toc_length=4bytes = 103byte is starting point
  let start = 103
  let end = start + 1 // 104
  const tocLength = new DataView(
    await blobToBuffer(file.slice(99, 103) ?? new Blob()),
  ).getUint32(0, false)
  await getPositions(start, end, file, positions, tocLength as any)

  const data = await blobToBuffer(
    file.slice(
      Number(positions['manifest'][0]),
      Number(positions['manifest'][0]) + Number(positions['manifest'][1]),
    ),
  )

  return {
    ...(await cbor.decode(data, true)),
    icon: await blobToDataURL(
      file.slice(
        Number(positions['icon'][0]),
        Number(positions['icon'][0]) + Number(positions['icon'][1]),
        '',
      ),
    ),
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
  return a.length === b.length && a.every((value, index) => value === b[index])
}
