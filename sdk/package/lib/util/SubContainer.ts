import * as fs from "fs/promises"
import * as T from "../../../base/lib/types"
import * as cp from "child_process"
import { promisify } from "util"
import { Buffer } from "node:buffer"
import { once } from "../../../base/lib/util/once"
import { Drop } from "./Drop"
import { Mounts } from "../mainFn/Mounts"
import { BackupEffects } from "../backup/Backups"

export const execFile = promisify(cp.execFile)
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
  get destroy(): undefined | (() => Promise<null>)
  exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
  ): Promise<ExecResults>
  spawn(
    command: string[],
    options?: CommandOptions & StdioOptions,
  ): Promise<cp.ChildProcess>
}
/**
 * Want to limit what we can do in a container, so we want to launch a container with a specific image and the mounts.
 *
 * Implements:
 * @see {@link ExecSpawnable}
 */
export class SubContainer<
    Manifest extends T.SDKManifest,
    Effects extends T.Effects = T.Effects,
  >
  extends Drop
  implements ExecSpawnable
{
  private destroyed = false

  private leader: cp.ChildProcess
  private leaderExited: boolean = false
  private waitProc: () => Promise<null>
  private constructor(
    readonly effects: Effects,
    readonly imageId: T.ImageId,
    readonly rootfs: string,
    readonly guid: T.Guid,
  ) {
    super()
    this.leaderExited = false
    this.leader = cp.spawn("start-cli", ["subcontainer", "launch", rootfs], {
      killSignal: "SIGKILL",
      stdio: "inherit",
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
              return reject(
                new Error(`Failed to start subcontainer ${this.imageId}`),
              )
            }
            await wait(1)
          }
          resolve(null)
        }),
    )
  }
  static async of<Manifest extends T.SDKManifest, Effects extends T.Effects>(
    effects: Effects,
    image: {
      imageId: keyof Manifest["images"] & T.ImageId
      sharedRun?: boolean
    },
    mounts:
      | (Effects extends BackupEffects
          ? Mounts<
              Manifest,
              {
                subpath: string | null
                mountpoint: string
              }
            >
          : Mounts<Manifest, never>)
      | null,
    name: string,
  ) {
    const { imageId, sharedRun } = image
    const [rootfs, guid] = await effects.subcontainer.createFs({
      imageId,
      name,
    })

    const res = new SubContainer(effects, imageId, rootfs, guid)

    try {
      if (mounts) {
        await res.mount(mounts)
      }
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

      return res
    } catch (e) {
      await res.destroy()
      throw e
    }
  }

  static async withTemp<
    Manifest extends T.SDKManifest,
    T,
    Effects extends T.Effects,
  >(
    effects: Effects,
    image: {
      imageId: keyof Manifest["images"] & T.ImageId
      sharedRun?: boolean
    },
    mounts:
      | (Effects extends BackupEffects
          ? Mounts<
              Manifest,
              {
                subpath: string | null
                mountpoint: string
              }
            >
          : Mounts<Manifest, never>)
      | null,
    name: string,
    fn: (subContainer: SubContainer<Manifest, Effects>) => Promise<T>,
  ): Promise<T> {
    const subContainer = await SubContainer.of(effects, image, mounts, name)
    try {
      return await fn(subContainer)
    } finally {
      await subContainer.destroy()
    }
  }

  async mount(
    mounts: Effects extends BackupEffects
      ? Mounts<
          Manifest,
          {
            subpath: string | null
            mountpoint: string
          }
        >
      : Mounts<Manifest, never>,
  ): Promise<SubContainer<Manifest, Effects>> {
    for (let mount of mounts.build()) {
      let { options, mountpoint } = mount
      const path = mountpoint.startsWith("/")
        ? `${this.rootfs}${mountpoint}`
        : `${this.rootfs}/${mountpoint}`
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
        const from = `/media/startos/assets/${subpath}`

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
    }
    return this
  }

  private async killLeader() {
    if (this.leaderExited) {
      return
    }
    return new Promise<null>((resolve, reject) => {
      try {
        let timeout = setTimeout(() => this.leader.kill("SIGKILL"), 30000)
        this.leader.on("exit", () => {
          clearTimeout(timeout)
          resolve(null)
        })
        if (!this.leader.kill("SIGTERM")) {
          reject(new Error("kill(2) failed"))
        }
      } catch (e) {
        reject(e)
      }
    })
  }

  get destroy() {
    return async () => {
      if (!this.destroyed) {
        const guid = this.guid
        await this.killLeader()
        await this.effects.subcontainer.destroyFs({ guid })
        this.destroyed = true
      }
      return null
    }
  }

  onDrop(): void {
    console.log(`Cleaning up dangling subcontainer ${this.guid}`)
    this.destroy()
  }

  /**
   * @description run a command inside this subcontainer
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
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
    let user = imageMeta.user || "root"
    if (options?.user) {
      user = options.user
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
        `--user=${user}`,
        `--workdir=${workdir}`,
        ...extra,
        this.rootfs,
        ...command,
      ],
      options || {},
    )
    if (options?.input) {
      await new Promise<null>((resolve, reject) => {
        try {
          child.stdin.on("error", (e) => reject(e))
          child.stdin.write(options.input, (e) => {
            if (e) {
              reject(e)
            } else {
              resolve(null)
            }
          })
        } catch (e) {
          reject(e)
        }
      })
      await new Promise<null>((resolve, reject) => {
        try {
          child.stdin.end(resolve)
        } catch (e) {
          reject(e)
        }
      })
    }
    const stdout = { data: "" as string }
    const stderr = { data: "" as string }
    const appendData =
      (appendTo: { data: string }) => (chunk: string | Buffer | any) => {
        if (typeof chunk === "string" || chunk instanceof Buffer) {
          appendTo.data += chunk.toString()
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
    const imageMeta: T.ImageMetadata = await fs
      .readFile(`/media/startos/images/${this.imageId}.json`, {
        encoding: "utf8",
      })
      .catch(() => "{}")
      .then(JSON.parse)
    let extra: string[] = []
    let user = imageMeta.user || "root"
    if (options?.user) {
      user = options.user
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
        `--user=${user}`,
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
    options: CommandOptions & StdioOptions = { stdio: "inherit" },
  ): Promise<cp.ChildProcess> {
    await this.waitProc()
    const imageMeta: T.ImageMetadata = await fs
      .readFile(`/media/startos/images/${this.imageId}.json`, {
        encoding: "utf8",
      })
      .catch(() => "{}")
      .then(JSON.parse)
    let extra: string[] = []
    let user = imageMeta.user || "root"
    if (options?.user) {
      user = options.user
      delete options.user
    }
    let workdir = imageMeta.workdir || "/"
    if (options.cwd) {
      workdir = options.cwd
      delete options.cwd
    }
    return cp.spawn(
      "start-cli",
      [
        "subcontainer",
        "exec",
        `--env=/media/startos/images/${this.imageId}.env`,
        `--user=${user}`,
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
    options: CommandOptions & StdioOptions = { stdio: "inherit" },
  ): Promise<cp.ChildProcess> {
    return this.subContainer.spawn(command, options)
  }
}

export type CommandOptions = {
  /**
   * Environment variables to set for this command
   */
  env?: { [variable: string]: string }
  /**
   * the working directory to run this command in
   */
  cwd?: string
  /**
   * the user to run this command as
   */
  user?: string
}

export type StdioOptions = {
  stdio?: cp.IOType
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

export class ExitError extends Error {
  constructor(
    readonly command: string,
    readonly result: {
      exitCode: number | null
      exitSignal: T.Signals | null
      stdout: string | Buffer
      stderr: string | Buffer
    },
  ) {
    let message: string
    if (result.exitCode) {
      message = `${command} failed with exit code ${result.exitCode}: ${result.stderr}`
    } else if (result.exitSignal) {
      message = `${command} terminated with signal ${result.exitSignal}: ${result.stderr}`
    } else {
      message = `${command} succeeded: ${result.stdout}`
    }
    super(message)
  }
}
