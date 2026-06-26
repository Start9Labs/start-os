import * as fs from 'node:fs/promises'
import * as T from '../../../base/lib/types'

/**
 * Common interface for objects that have a subpath method (Volume, SubContainer, etc.)
 */
export interface PathBase {
  subpath(path: string): string
}

/**
 * @description Represents a volume in the StartOS filesystem.
 * Provides utilities for reading and writing files within the volume.
 */
export class Volume<Id extends string = string> implements PathBase {
  /**
   * The absolute path to this volume's root directory
   */
  readonly path: string

  constructor(readonly id: Id) {
    this.path = `/media/startos/volumes/${id}`
  }

  /**
   * Get the absolute path to a file or directory within this volume
   * @param subpath Path relative to the volume root
   */
  subpath(subpath: string): string {
    return subpath.startsWith('/')
      ? `${this.path}${subpath}`
      : `${this.path}/${subpath}`
  }

  /**
   * @description Read a file from this volume
   * @param subpath Path relative to the volume root (e.g. "config.json" or "/data/file.txt")
   * @param options Optional read options (same as node:fs/promises readFile)
   */
  async readFile(
    subpath: string,
    options?: Parameters<typeof fs.readFile>[1],
  ): Promise<Buffer | string> {
    const fullPath = this.subpath(subpath)
    return fs.readFile(fullPath, options)
  }

  /**
   * @description Write a file to this volume
   * @param subpath Path relative to the volume root (e.g. "config.json" or "/data/file.txt")
   * @param data The data to write
   * @param options Optional write options (same as node:fs/promises writeFile)
   */
  async writeFile(
    subpath: string,
    data:
      | string
      | NodeJS.ArrayBufferView
      | Iterable<string | NodeJS.ArrayBufferView>
      | AsyncIterable<string | NodeJS.ArrayBufferView>,
    options?: Parameters<typeof fs.writeFile>[2],
  ): Promise<void> {
    const fullPath = this.subpath(subpath)
    const dir = fullPath.replace(/\/[^/]*\/?$/, '')
    await fs.mkdir(dir, { recursive: true })
    return fs.writeFile(fullPath, data, options)
  }
}

/**
 * Type-safe volumes object that provides Volume instances for each volume defined in the manifest
 */
export type Volumes<Manifest extends T.SDKManifest> = {
  [K in Manifest['volumes'][number]]: Volume<K>
}

/**
 * Creates a type-safe volumes object from a manifest
 */
export function createVolumes<Manifest extends T.SDKManifest>(
  manifest: Manifest,
): Volumes<Manifest> {
  const volumes = {} as Volumes<Manifest>
  for (const volumeId of manifest.volumes) {
    ;(volumes as any)[volumeId] = new Volume(volumeId)
  }
  return volumes
}
