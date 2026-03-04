import {
  DataUrl,
  DependencyMetadata,
  Manifest,
  MerkleArchiveCommitment,
  PackageId,
} from '../osBindings'
import { ArrayBufferReader, MerkleArchive } from './merkleArchive'
import mime from 'mime'
import { DirectoryContents } from './merkleArchive/directoryContents'
import { FileContents } from './merkleArchive/fileContents'

const magicAndVersion = new Uint8Array([59, 59, 2])

/**
 * Compares two `Uint8Array` instances byte-by-byte for equality.
 *
 * @returns `true` if both arrays have the same length and identical bytes
 */
export function compare(a: Uint8Array, b: Uint8Array) {
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false
  }
  return true
}

/**
 * Represents a parsed `.s9pk` package archive — the binary distribution format for StartOS services.
 *
 * An `S9pk` wraps a verified {@link Manifest}, a {@link MerkleArchive} containing the package's
 * assets (icon, license, dependency metadata), and the total archive size in bytes.
 *
 * @example
 * ```ts
 * const s9pk = await S9pk.deserialize(file, null)
 * console.log(s9pk.manifest.id)    // e.g. "bitcoind"
 * console.log(s9pk.size)           // archive size in bytes
 * const icon = await s9pk.icon()   // base64 data URL
 * const license = await s9pk.license()
 * ```
 */
export class S9pk {
  private constructor(
    /** The parsed package manifest containing metadata, dependencies, and interface definitions. */
    readonly manifest: Manifest,
    /** The Merkle-verified archive containing the package's files. */
    readonly archive: MerkleArchive,
    /** The total size of the archive in bytes. */
    readonly size: number,
  ) {}
  /**
   * Deserializes an `S9pk` from a `Blob` (e.g. a `File` from a browser file input).
   *
   * Validates the magic bytes and version header, then parses the Merkle archive structure.
   * If a `commitment` is provided, the archive is cryptographically verified against it.
   *
   * @param source - The raw `.s9pk` file as a `Blob`
   * @param commitment - An optional Merkle commitment to verify the archive against, or `null` to skip verification
   * @returns A fully parsed `S9pk` instance
   * @throws If the magic bytes are invalid or the archive fails verification
   */
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
      throw new Error('Invalid Magic or Unexpected Version')
    }

    const archive = await MerkleArchive.deserialize(
      source,
      's9pk',
      header,
      commitment,
    )

    const manifest = JSON.parse(
      new TextDecoder().decode(
        await archive.contents
          .getPath(['manifest.json'])
          ?.verifiedFileContents(),
      ),
    )

    return new S9pk(manifest, archive, source.size)
  }
  /**
   * Extracts the package icon from the archive and returns it as a base64-encoded data URL.
   *
   * Looks for a file named `icon.*` with an image MIME type (e.g. `icon.png`, `icon.svg`).
   *
   * @returns A data URL string like `"data:image/png;base64,..."` suitable for use in `<img src>`.
   * @throws If no icon file is found in the archive
   */
  async icon(): Promise<DataUrl> {
    const iconName = Object.keys(this.archive.contents.contents).find(
      (name) =>
        name.startsWith('icon.') && mime.getType(name)?.startsWith('image/'),
    )
    if (!iconName) {
      throw new Error('no icon found in archive')
    }
    return (
      `data:${mime.getType(iconName)};base64,` +
      Buffer.from(
        await this.archive.contents.getPath([iconName])!.verifiedFileContents(),
      ).toString('base64')
    )
  }

  /**
   * Returns the metadata (e.g. `{ title }`) for a specific dependency by its package ID.
   *
   * @param id - The dependency's package identifier (e.g. `"bitcoind"`)
   * @returns The dependency metadata object, or `null` if the dependency is not present in the archive
   */
  async dependencyMetadataFor(id: PackageId) {
    const entry = this.archive.contents.getPath([
      'dependencies',
      id,
      'metadata.json',
    ])
    if (!entry) return null
    return JSON.parse(
      new TextDecoder().decode(await entry.verifiedFileContents()),
    ) as { title: string }
  }

  /**
   * Returns the icon for a specific dependency as a base64 data URL.
   *
   * @param id - The dependency's package identifier
   * @returns A data URL string, or `null` if the dependency or its icon is not present
   */
  async dependencyIconFor(id: PackageId) {
    const dir = this.archive.contents.getPath(['dependencies', id])
    if (!dir || !(dir.contents instanceof DirectoryContents)) return null
    const iconName = Object.keys(dir.contents.contents).find(
      (name) =>
        name.startsWith('icon.') && mime.getType(name)?.startsWith('image/'),
    )
    if (!iconName) return null
    return (
      `data:${mime.getType(iconName)};base64,` +
      Buffer.from(
        await dir.contents.getPath([iconName])!.verifiedFileContents(),
      ).toString('base64')
    )
  }

  /**
   * Returns a merged record of all dependency metadata (title, icon, description, optional flag)
   * for every dependency declared in the manifest.
   *
   * @returns A record keyed by package ID, each containing `{ title, icon, description, optional }`
   */
  async dependencyMetadata() {
    return Object.fromEntries(
      await Promise.all(
        Object.entries(this.manifest.dependencies)
          .filter(([_, info]) => !!info)
          .map(async ([id, info]) => [
            id,
            {
              ...(await this.dependencyMetadataFor(id)),
              icon: await this.dependencyIconFor(id),
              description: info!.description,
              optional: info!.optional,
            },
          ]),
      ),
    )
  }

  /**
   * Reads and returns the `LICENSE.md` file from the archive as a UTF-8 string.
   *
   * @returns The full license text
   * @throws If `LICENSE.md` is not found in the archive
   */
  async license(): Promise<string> {
    const file = this.archive.contents.getPath(['LICENSE.md'])
    if (!file || !(file.contents instanceof FileContents))
      throw new Error('license.md not found in archive')
    return new TextDecoder().decode(await file.verifiedFileContents())
  }
}
