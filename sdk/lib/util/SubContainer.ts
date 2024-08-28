import * as fs from "fs/promises"
import * as T from "../types"
import * as cp from "child_process"
import { promisify } from "util"
import { Buffer } from "node:buffer"
import { once } from "./once"
export const execFile = promisify(cp.execFile)
const WORKDIR = (imageId: string) => `/media/startos/images/${imageId}/`
const False = () => false
type ExecResults = {
  exitCode: number | null
  exitSignal: NodeJS.Signals | null
  stdout: string | Buffer
  stderr: string | Buffer
}

export type ExecOptions = {
  input?: string | Buffer
}

const TIMES_TO_WAIT_FOR_PROC = 100

/**
 * This is the type that is going to describe what an subcontainer could do. The main point of the
 * subcontainer is to have commands that run in a chrooted environment. This is useful for running
 * commands in a containerized environment. But, I wanted the destroy to sometimes be doable, for example the
 * case where the subcontainer isn't owned by the process, the subcontainer shouldn't be destroyed.
 */
export interface ExecSpawnable {
  get destroy(): undefined | (() => Promise<void>)
  exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
  ): Promise<ExecResults>
  spawn(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams>
}
/**
 * Want to limit what we can do in a container, so we want to launch a container with a specific image and the mounts.
 *
 * Implements:
 * @see {@link ExecSpawnable}
 */
export class SubContainer implements ExecSpawnable {
  private leader: cp.ChildProcess
  private leaderExited: boolean = false
  private waitProc: () => Promise<void>
  private constructor(
    readonly effects: T.Effects,
    readonly imageId: T.ImageId,
    readonly rootfs: string,
    readonly guid: T.Guid,
  ) {
    this.leaderExited = false
    this.leader = cp.spawn("start-cli", ["subcontainer", "launch", rootfs], {
      killSignal: "SIGKILL",
      stdio: "ignore",
    })
    this.leader.on("exit", () => {
      this.leaderExited = true
    })
    this.waitProc = once(
      () =>
        new Promise(async (resolve, reject) => {
          let count = 0
          while (
            !(await fs.stat(`${this.rootfs}/proc/1`).then((x) => !!x, False))
          ) {
            if (count++ > TIMES_TO_WAIT_FOR_PROC) {
              console.debug("Failed to start subcontainer", {
                guid: this.guid,
                imageId: this.imageId,
                rootfs: this.rootfs,
              })
              reject(new Error(`Failed to start subcontainer ${this.imageId}`))
            }
            await wait(1)
          }
          resolve()
        }),
    )
  }
  static async of(
    effects: T.Effects,
    image: { id: T.ImageId; sharedRun?: boolean },
  ) {
    const { id, sharedRun } = image
    const [rootfs, guid] = await effects.subcontainer.createFs({
      imageId: id as string,
    })

    const shared = ["dev", "sys"]
    if (!!sharedRun) {
      shared.push("run")
    }

    await fs.mkdir(`${rootfs}/etc`, { recursive: true })
    await fs.copyFile("/etc/resolv.conf", `${rootfs}/etc/resolv.conf`)

    for (const dirPart of shared) {
      const from = `/${dirPart}`
      const to = `${rootfs}/${dirPart}`
      await fs.mkdir(from, { recursive: true })
      await fs.mkdir(to, { recursive: true })
      await execFile("mount", ["--rbind", from, to])
    }

    return new SubContainer(effects, id, rootfs, guid)
  }

  static async with<T>(
    effects: T.Effects,
    image: { id: T.ImageId; sharedRun?: boolean },
    mounts: { options: MountOptions; path: string }[],
    fn: (subContainer: SubContainer) => Promise<T>,
  ): Promise<T> {
    const subContainer = await SubContainer.of(effects, image)
    try {
      for (let mount of mounts) {
        await subContainer.mount(mount.options, mount.path)
      }
      return await fn(subContainer)
    } finally {
      await subContainer.destroy()
    }
  }

  async mount(options: MountOptions, path: string): Promise<SubContainer> {
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

  private async killLeader() {
    if (this.leaderExited) {
      return
    }
    return new Promise<void>((resolve, reject) => {
      try {
        this.leader.on("exit", () => {
          resolve()
        })
        if (!this.leader.kill("SIGKILL")) {
          reject(new Error("kill(2) failed"))
        }
      } catch (e) {
        reject(e)
      }
    })
  }

  get destroy() {
    return async () => {
      const guid = this.guid
      await this.killLeader()
      await this.effects.subcontainer.destroyFs({ guid })
    }
  }

  async exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs: number | null = 30000,
  ): Promise<{
    exitCode: number | null
    exitSignal: NodeJS.Signals | null
    stdout: string | Buffer
    stderr: string | Buffer
  }> {
    await this.waitProc()
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
        "subcontainer",
        "exec",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      options || {},
    )
    if (options?.input) {
      await new Promise<void>((resolve, reject) =>
        child.stdin.write(options.input, (e) => {
          if (e) {
            reject(e)
          } else {
            resolve()
          }
        }),
      )
      await new Promise<void>((resolve) => child.stdin.end(resolve))
    }
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
      let killTimeout: NodeJS.Timeout | undefined
      if (timeoutMs !== null && child.pid) {
        killTimeout = setTimeout(() => child.kill("SIGKILL"), timeoutMs)
      }
      child.stdout.on("data", appendData(stdout))
      child.stderr.on("data", appendData(stderr))
      child.on("exit", (code, signal) => {
        clearTimeout(killTimeout)
        resolve({
          exitCode: code,
          exitSignal: signal,
          stdout: stdout.data,
          stderr: stderr.data,
        })
      })
    })
  }

  async launch(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    await this.waitProc()
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
    await this.killLeader()
    this.leaderExited = false
    this.leader = cp.spawn(
      "start-cli",
      [
        "subcontainer",
        "launch",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      { ...options, stdio: "inherit" },
    )
    this.leader.on("exit", () => {
      this.leaderExited = true
    })
    return this.leader as cp.ChildProcessWithoutNullStreams
  }

  async spawn(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    await this.waitProc()
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
        "subcontainer",
        "exec",
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

/**
 * Take an subcontainer but remove the ability to add the mounts and the destroy function.
 * Lets other functions, like health checks, to not destroy the parents.
 *
 */
export class SubContainerHandle implements ExecSpawnable {
  constructor(private subContainer: ExecSpawnable) {}
  get destroy() {
    return undefined
  }

  exec(
    command: string[],
    options?: CommandOptions,
    timeoutMs?: number | null,
  ): Promise<ExecResults> {
    return this.subContainer.exec(command, options, timeoutMs)
  }
  spawn(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    return this.subContainer.spawn(command, options)
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
function wait(time: number) {
  return new Promise((resolve) => setTimeout(resolve, time))
}
