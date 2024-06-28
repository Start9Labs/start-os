import { MerkleArchiveCommitment } from "../../osBindings"
import { DirectoryContents } from "./directoryContents"
import { FileContents } from "./fileContents"
import { ed25519ctx } from "@noble/curves/ed25519"
import { VarIntProcessor } from "./varint"

const subtleCrypto = new globalThis.Crypto().subtle

const maxVarstringLen = 1024 * 1024

export type Signer = {
  pubkey: Uint8Array
  signature: Uint8Array
  maxSize: bigint
  context: string
}

export class ArrayBufferReader {
  constructor(private buffer: ArrayBuffer) {}
  next(length: number): ArrayBuffer {
    const res = this.buffer.slice(0, length)
    this.buffer = this.buffer.slice(length)
    return res
  }
  nextU64(): bigint {
    return new DataView(this.next(8)).getBigUint64(0)
  }
  nextVarint(): number {
    const p = new VarIntProcessor()
    while (!p.finished) {
      p.push(new Uint8Array(this.buffer.slice(0, 1))[0])
      this.buffer = this.buffer.slice(1)
    }
    const res = p.decode()
    if (res === null) {
      throw new Error("Reached EOF")
    }
    return res
  }
  nextVarstring(): string {
    const len = Math.min(this.nextVarint(), maxVarstringLen)
    return new TextDecoder().decode(this.next(len))
  }
}

export class MerkleArchive {
  static readonly headerSize =
    32 + // pubkey
    64 + // signature
    32 + //sighash
    8 + //size
    DirectoryContents.headerSize
  private constructor(
    readonly signer: Signer,
    readonly contents: DirectoryContents,
  ) {}
  static async deserialize(
    source: Blob,
    context: string,
    header: ArrayBufferReader,
    commitment: MerkleArchiveCommitment | null,
  ): Promise<MerkleArchive> {
    const pubkey = new Uint8Array(header.next(32))
    const signature = new Uint8Array(header.next(64))
    const sighash = header.next(32)
    const rootMaxSizeBytes = header.next(8)
    const maxSize = header.nextU64()

    if (
      !ed25519ctx.verify(
        signature,
        new Uint8Array(
          await new Blob([sighash, rootMaxSizeBytes]).arrayBuffer(),
        ),
        pubkey,
        {
          context: new TextEncoder().encode(context),
          zip215: true,
        },
      )
    ) {
      throw new Error("signature verification failed")
    }

    if (commitment) {
      if (sighash !== Buffer.from(commitment.rootSighash, "base64").buffer) {
        throw new Error("merkle root mismatch")
      }
      if (maxSize > commitment.rootMaxsize) {
        throw new Error("root directory max size too large")
      }
    } else if (maxSize > 1024 * 1024) {
      throw new Error(
        "root directory max size over 1MiB, cancelling download in case of DOS attack",
      )
    }

    const contents = await DirectoryContents.deserialize(
      source,
      header,
      sighash,
      maxSize,
    )

    return new MerkleArchive(
      {
        pubkey,
        signature,
        maxSize,
        context,
      },
      contents,
    )
  }
}

export class Entry {
  private constructor(
    readonly hash: ArrayBuffer,
    readonly size: bigint,
    readonly contents: EntryContents,
  ) {}
  static async deserialize(
    source: Blob,
    header: ArrayBufferReader,
  ): Promise<Entry> {
    const hash = header.next(32)
    const size = header.nextU64()
    const contents = await deserializeEntryContents(source, header, hash, size)

    return new Entry(hash, size, contents)
  }
  async verifiedFileContents(): Promise<ArrayBuffer> {
    if (!this.contents) {
      throw new Error("file is missing from archive")
    }
    if (!(this.contents instanceof FileContents)) {
      throw new Error("is not a regular file")
    }
    return this.contents.verified(this.hash)
  }
}

export type EntryContents = null | FileContents | DirectoryContents
async function deserializeEntryContents(
  source: Blob,
  header: ArrayBufferReader,
  hash: ArrayBuffer,
  size: bigint,
): Promise<EntryContents> {
  const typeId = new Uint8Array(header.next(1))[0]
  switch (typeId) {
    case 0:
      return null
    case 1:
      return FileContents.deserialize(source, header, size)
    case 2:
      return DirectoryContents.deserialize(source, header, hash, size)
    default:
      throw new Error(`Unknown type id ${typeId} found in MerkleArchive`)
  }
}
