import {
  DataUrl,
  DependencyMetadata,
  Manifest,
  MerkleArchiveCommitment,
  PackageId,
} from "../osBindings"
import { ArrayBufferReader, MerkleArchive } from "./merkleArchive"
import mime from "mime"
import { DirectoryContents } from "./merkleArchive/directoryContents"
import { FileContents } from "./merkleArchive/fileContents"

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

    return new S9pk(manifest, archive, source.size)
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

  async dependencyMetadataFor(id: PackageId) {
    const entry = this.archive.contents.getPath([
      "dependencies",
      id,
      "metadata.json",
    ])
    if (!entry) return null
    return JSON.parse(
      new TextDecoder().decode(await entry.verifiedFileContents()),
    ) as { title: string }
  }

  async dependencyIconFor(id: PackageId) {
    const dir = this.archive.contents.getPath(["dependencies", id])
    if (!dir || !(dir.contents instanceof DirectoryContents)) return null
    const iconName = Object.keys(dir.contents.contents).find(
      (name) =>
        name.startsWith("icon.") && mime.getType(name)?.startsWith("image/"),
    )
    if (!iconName) return null
    return (
      `data:${mime.getType(iconName)};base64,` +
      Buffer.from(
        await dir.contents.getPath([iconName])!.verifiedFileContents(),
      ).toString("base64")
    )
  }

  async dependencyMetadata(): Promise<Record<PackageId, DependencyMetadata>> {
    return Object.fromEntries(
      await Promise.all(
        Object.entries(this.manifest.dependencies).map(async ([id, info]) => [
          id,
          {
            ...(await this.dependencyMetadataFor(id)),
            icon: await this.dependencyIconFor(id),
            description: info.description,
            optional: info.optional,
          },
        ]),
      ),
    )
  }

  async instructions(): Promise<string> {
    const file = this.archive.contents.getPath(["instructions.md"])
    if (!file || !(file.contents instanceof FileContents))
      throw new Error("instructions.md not found in archive")
    return new TextDecoder().decode(await file.verifiedFileContents())
  }

  async license(): Promise<string> {
    const file = this.archive.contents.getPath(["LICENSE.md"])
    if (!file || !(file.contents instanceof FileContents))
      throw new Error("instructions.md not found in archive")
    return new TextDecoder().decode(await file.verifiedFileContents())
  }
}
