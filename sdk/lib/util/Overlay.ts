import * as fs from "fs/promises"
import * as T from "../types"
import * as cp from "child_process"
import { promisify } from "util"
import { Buffer } from "node:buffer"
export const execFile = promisify(cp.execFile)
const WORKDIR = (imageId: string) => `/media/startos/images/${imageId}/`
export class Overlay {
  private destroyed = false
  private constructor(
    readonly effects: T.Effects,
    readonly imageId: T.ImageId,
    readonly rootfs: string,
    readonly guid: T.Guid,
  ) {}
  static async of(
    effects: T.Effects,
    image: { id: T.ImageId; sharedRun?: boolean },
  ) {
    const { id, sharedRun } = image
    const [rootfs, guid] = await effects.createOverlayedImage({
      imageId: id as string,
    })

    const shared = ["dev", "sys", "proc"]
    if (!!sharedRun) {
      shared.push("run")
    }

    fs.copyFile("/etc/resolv.conf", `${rootfs}/etc/resolv.conf`)

    for (const dirPart of shared) {
      const from = `/${dirPart}`
      const to = `${rootfs}/${dirPart}`
      await fs.mkdir(from, { recursive: true })
      await fs.mkdir(to, { recursive: true })
      await execFile("mount", ["--rbind", from, to])
    }

    return new Overlay(effects, id, rootfs, guid)
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
      const from = `/media/startos/volumes/${options.id}${subpath}`

      await fs.mkdir(from, { recursive: true })
      await fs.mkdir(path, { recursive: true })
      await execFile("mount", ["--bind", from, path])
    } else if (options.type === "assets") {
      const subpath = options.subpath
        ? options.subpath.startsWith("/")
          ? options.subpath
          : `/${options.subpath}`
        : "/"
      const from = `/media/startos/assets/${options.id}${subpath}`

      await fs.mkdir(from, { recursive: true })
      await fs.mkdir(path, { recursive: true })
      await execFile("mount", ["--bind", from, path])
    } else if (options.type === "pointer") {
      await this.effects.mount({ location: path, target: options })
    } else if (options.type === "backup") {
      const subpath = options.subpath
        ? options.subpath.startsWith("/")
          ? options.subpath
          : `/${options.subpath}`
        : "/"
      const from = `/media/startos/backup${subpath}`

      await fs.mkdir(from, { recursive: true })
      await fs.mkdir(path, { recursive: true })
      await execFile("mount", ["--bind", from, path])
    } else {
      throw new Error(`unknown type ${(options as any).type}`)
    }
    return this
  }

  async destroy() {
    if (this.destroyed) return
    this.destroyed = true
    const imageId = this.imageId
    const guid = this.guid
    await this.effects.destroyOverlayedImage({ guid })
  }

  async exec(
    command: string[],
    options?: CommandOptions,
    timeoutMs: number | null = 30000,
  ): Promise<{
    exitCode: number | null
    exitSignal: NodeJS.Signals | null
    stdout: string | Buffer
    stderr: string | Buffer
  }> {
    const imageMeta: T.ImageMetadata = await fs
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
    const child = cp.spawn(
      "start-cli",
      [
        "chroot",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      options || {},
    )
    const pid = child.pid
    const stdout = { data: "" as string | Buffer }
    const stderr = { data: "" as string | Buffer }
    const appendData =
      (appendTo: { data: string | Buffer }) =>
      (chunk: string | Buffer | any) => {
        if (typeof appendTo.data === "string" && typeof chunk === "string") {
          appendTo.data += chunk
        } else if (typeof chunk === "string" || chunk instanceof Buffer) {
          appendTo.data = Buffer.concat([
            Buffer.from(appendTo.data),
            Buffer.from(chunk),
          ])
        } else {
          console.error("received unexpected chunk", chunk)
        }
      }
    return new Promise((resolve, reject) => {
      child.on("error", reject)
      if (timeoutMs !== null && pid) {
        setTimeout(
          () => execFile("pkill", ["-9", "-s", String(pid)]).catch((_) => {}),
          timeoutMs,
        )
      }
      child.stdout.on("data", appendData(stdout))
      child.stderr.on("data", appendData(stderr))
      child.on("exit", (code, signal) =>
        resolve({
          exitCode: code,
          exitSignal: signal,
          stdout: stdout.data,
          stderr: stderr.data,
        }),
      )
    })
  }

  async spawn(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    const imageMeta: any = await fs
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
  | MountOptionsBackup

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

export type MountOptionsBackup = {
  type: "backup"
  subpath: string | null
}
