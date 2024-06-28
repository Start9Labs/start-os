import { DataUrl, Manifest, MerkleArchiveCommitment } from "../osBindings"
import { ArrayBufferReader, MerkleArchive } from "./merkleArchive"
import mime from "mime"

const magicAndVersion = new Uint8Array([59, 59, 2])

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
    const magicVersion = header.next(magicAndVersion.length)
    if (magicVersion !== magicAndVersion) {
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
  async icon(): Promise<DataUrl> {
    const iconName = Object.keys(this.archive.contents.contents).find(
      (name) =>
        name.startsWith("icon.") && mime.getType(name)?.startsWith("image/"),
    )
    if (!iconName) {
      throw new Error("no icon found in archive")
    }
    return (
      `data:${mime.getType(iconName)};base64,` +
      Buffer.from(
        await this.archive.contents.getPath([iconName])!.verifiedFileContents(),
      ).toString("base64")
    )
  }
}
