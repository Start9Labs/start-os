import { Manifest, MerkleArchiveCommitment } from "../osBindings"
import { ArrayBufferReader, MerkleArchive } from "./merkleArchive"

const magicAndVersion = new Uint8Array([59, 59, 2])

export function compare(a: Uint8Array, b: Uint8Array) {
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false
  }
  return true
}

export class S9pk {
  private constructor(
    readonly manifest: Manifest,
    readonly archive: MerkleArchive,
    readonly size: number,
  ) {}
  static async deserialize(
    source: Blob,
    commitment: MerkleArchiveCommitment | null,
  ): Promise<S9pk> {
    const header = new ArrayBufferReader(
      await source
        .slice(0, magicAndVersion.length + MerkleArchive.headerSize)
        .arrayBuffer(),
    )
    const magicVersion = new Uint8Array(header.next(magicAndVersion.length))
    if (!compare(magicVersion, magicAndVersion)) {
      throw new Error("Invalid Magic or Unexpected Version")
    }

    const archive = await MerkleArchive.deserialize(
      source,
      "s9pk",
      header,
      commitment,
    )

    const manifest = JSON.parse(
      new TextDecoder().decode(
        await archive.contents
          .getPath(["manifest.json"])
          ?.verifiedFileContents(),
      ),
    )

    return new S9pk(manifest, archive, source.length)
  }
}
