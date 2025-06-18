import { blake3 } from "@noble/hashes/blake3"
import { ArrayBufferReader } from "."
import { compare } from ".."

export class FileContents {
  private constructor(readonly contents: Blob) {}
  static deserialize(
    source: Blob,
    header: ArrayBufferReader,
    size: bigint,
  ): FileContents {
    const position = header.nextU64()
    return new FileContents(
      source.slice(Number(position), Number(position + size)),
    )
  }
  async verified(hash: Uint8Array): Promise<ArrayBuffer> {
    const res = await this.contents.arrayBuffer()
    if (!compare(hash, blake3(new Uint8Array(res)))) {
      throw new Error("hash sum mismatch")
    }
    return res
  }
}
