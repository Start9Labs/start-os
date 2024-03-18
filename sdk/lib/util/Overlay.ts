import * as fs from "fs/promises"
import * as T from "../types"
import * as cp from "child_process"
import { promisify } from "util"
import { Buffer } from "node:buffer"
export const execFile = promisify(cp.execFile)
const WORKDIR = (imageId: string) => `/media/startos/images/${imageId}/`
export class Overlay {
  private constructor(
    readonly effects: T.Effects,
    readonly imageId: string,
    readonly rootfs: string,
    readonly guid: string,
  ) {}
  static async of(effects: T.Effects, imageId: string) {
    const [rootfs, guid] = await effects.createOverlayedImage({ imageId })

    for (const dirPart of ["dev", "sys", "proc", "run"] as const) {
      await fs.mkdir(`${rootfs}/${dirPart}`, { recursive: true })
      await execFile("mount", [
        "--rbind",
        `/${dirPart}`,
        `${rootfs}/${dirPart}`,
      ])
    }

    return new Overlay(effects, imageId, rootfs, guid)
  }

  async mount(options: MountOptions, path: string): Promise<Overlay> {
    path = path.startsWith("/")
      ? `${this.rootfs}${path}`
      : `${this.rootfs}/${path}`
    if (options.type === "volume") {
      const subpath = options.subpath
        ? options.subpath.startsWith("/")
          ? options.subpath
          : `/${options.subpath}`
        : "/"
      await execFile("mount", [
        "--bind",
        `/media/startos/volumes/${options.id}${subpath}`,
        path,
      ])
    } else if (options.type === "assets") {
      const subpath = options.subpath
        ? options.subpath.startsWith("/")
          ? options.subpath
          : `/${options.subpath}`
        : "/"
      await execFile("mount", [
        "--bind",
        `/media/startos/assets/${options.id}${subpath}`,
        path,
      ])
    } else if (options.type === "pointer") {
      await this.effects.mount({ location: path, target: options })
    } else {
      throw new Error(`unknown type ${(options as any).type}`)
    }
    return this
  }

  async destroy() {
    const imageId = this.imageId
    const guid = this.guid
    await this.effects.destroyOverlayedImage({ imageId, guid })
  }

  async exec(
    command: string[],
    options?: CommandOptions,
  ): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> {
    const imageMeta = await fs
      .readFile(`/media/startos/images/${this.imageId}.json`, {
        encoding: "utf8",
      })
      .catch(() => "{}")
      .then(JSON.parse)
    let extra: string[] = []
    if (options?.user) {
      extra.push(`--user=${options.user}`)
      delete options.user
    }
    let workdir = imageMeta.workdir || "/"
    if (options?.cwd) {
      workdir = options.cwd
      delete options.cwd
    }
    return await execFile(
      "start-cli",
      [
        "chroot",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      options,
    )
  }

  async spawn(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    const imageMeta = await fs
      .readFile(`/media/startos/images/${this.imageId}.json`, {
        encoding: "utf8",
      })
      .catch(() => "{}")
      .then(JSON.parse)
    let extra: string[] = []
    if (options?.user) {
      extra.push(`--user=${options.user}`)
      delete options.user
    }
    let workdir = imageMeta.workdir || "/"
    if (options?.cwd) {
      workdir = options.cwd
      delete options.cwd
    }
    return cp.spawn(
      "start-cli",
      [
        "chroot",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      options,
    )
  }
}

export type CommandOptions = {
  env?: { [variable: string]: string }
  cwd?: string
  user?: string
}

export type MountOptions =
  | MountOptionsVolume
  | MountOptionsAssets
  | MountOptionsPointer

export type MountOptionsVolume = {
  type: "volume"
  id: string
  subpath: string | null
  readonly: boolean
}

export type MountOptionsAssets = {
  type: "assets"
  id: string
  subpath: string | null
}

export type MountOptionsPointer = {
  type: "pointer"
  packageId: string
  volumeId: string
  subpath: string | null
  readonly: boolean
}
