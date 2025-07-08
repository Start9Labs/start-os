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

async function prepBind(
  from: string | null,
  to: string,
  type: "file" | "directory" | "infer",
) {
  const fromMeta = from ? await fs.stat(from).catch((_) => null) : null
  const toMeta = await fs.stat(to).catch((_) => null)

  if (type === "file" || (type === "infer" && from && fromMeta?.isFile())) {
    if (toMeta && toMeta.isDirectory()) await fs.rmdir(to, { recursive: false })
    if (from && !fromMeta) {
      await fs.mkdir(from.replace(/\/[^\/]*\/?$/, ""), { recursive: true })
      await fs.writeFile(from, "")
    }
    if (!toMeta) {
      await fs.mkdir(to.replace(/\/[^\/]*\/?$/, ""), { recursive: true })
      await fs.writeFile(to, "")
    }
  } else {
    if (toMeta && toMeta.isFile() && !toMeta.size) await fs.rm(to)
    if (from && !fromMeta) await fs.mkdir(from, { recursive: true })
    if (!toMeta) await fs.mkdir(to, { recursive: true })
  }
}

async function bind(
  from: string,
  to: string,
  type: "file" | "directory" | "infer",
) {
  await prepBind(from, to, type)

  await execFile("mount", ["--bind", from, to])
}

export interface SubContainer<
  Manifest extends T.SDKManifest,
  Effects extends T.Effects = T.Effects,
> extends Drop {
  readonly imageId: keyof Manifest["images"] & T.ImageId
  readonly rootfs: string
  readonly guid: T.Guid
  mount(
    mounts: Effects extends BackupEffects
      ? Mounts<
          Manifest,
          {
            subpath: string | null
            mountpoint: string
          }
        >
      : Mounts<Manifest, never>,
  ): Promise<this>

  destroy: () => Promise<null>

  /**
   * @description run a command inside this subcontainer
   * DOES NOT THROW ON NONZERO EXIT CODE (see execFail)
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
    abort?: AbortController,
  ): Promise<{
    throw: () => { stdout: string | Buffer; stderr: string | Buffer }
    exitCode: number | null
    exitSignal: NodeJS.Signals | null
    stdout: string | Buffer
    stderr: string | Buffer
  }>

  /**
   * @description run a command inside this subcontainer, throwing on non-zero exit status
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  execFail(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
    abort?: AbortController,
  ): Promise<{
    stdout: string | Buffer
    stderr: string | Buffer
  }>

  launch(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams>

  spawn(
    command: string[],
    options?: CommandOptions & StdioOptions,
  ): Promise<cp.ChildProcess>

  rc(): SubContainerRc<Manifest, Effects>

  isOwned(): this is SubContainerOwned<Manifest, Effects>
}

/**
 * Want to limit what we can do in a container, so we want to launch a container with a specific image and the mounts.
 */
export class SubContainerOwned<
    Manifest extends T.SDKManifest,
    Effects extends T.Effects = T.Effects,
  >
  extends Drop
  implements SubContainer<Manifest, Effects>
{
  private destroyed = false
  public rcs = 0

  private leader: cp.ChildProcess
  private leaderExited: boolean = false
  private waitProc: () => Promise<null>
  private constructor(
    readonly effects: Effects,
    readonly imageId: keyof Manifest["images"] & T.ImageId,
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
  ): Promise<SubContainerOwned<Manifest, Effects>> {
    const { imageId, sharedRun } = image
    const [rootfs, guid] = await effects.subcontainer.createFs({
      imageId,
      name,
    })

    const res = new SubContainerOwned(effects, imageId, rootfs, guid)

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
    const subContainer = await SubContainerOwned.of(
      effects,
      image,
      mounts,
      name,
    )
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
  ): Promise<this> {
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
        const from = `/media/startos/volumes/${options.volumeId}${subpath}`

        await bind(from, path, mount.options.filetype)
      } else if (options.type === "assets") {
        const subpath = options.subpath
          ? options.subpath.startsWith("/")
            ? options.subpath
            : `/${options.subpath}`
          : "/"
        const from = `/media/startos/assets/${subpath}`

        await bind(from, path, mount.options.filetype)
      } else if (options.type === "pointer") {
        await prepBind(null, path, options.filetype)
        await this.effects.mount({ location: path, target: options })
      } else if (options.type === "backup") {
        const subpath = options.subpath
          ? options.subpath.startsWith("/")
            ? options.subpath
            : `/${options.subpath}`
          : "/"
        const from = `/media/startos/backup${subpath}`

        await bind(from, path, mount.options.filetype)
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
   * DOES NOT THROW ON NONZERO EXIT CODE (see execFail)
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  async exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs: number | null = 30000,
    abort?: AbortController,
  ): Promise<{
    throw: () => { stdout: string | Buffer; stderr: string | Buffer }
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
    abort?.signal.addEventListener("abort", () => child.kill("SIGKILL"))
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
        const result = {
          exitCode: code,
          exitSignal: signal,
          stdout: stdout.data,
          stderr: stderr.data,
        }
        resolve({
          throw: () =>
            !code && !signal
              ? { stdout: stdout.data, stderr: stderr.data }
              : (() => {
                  throw new ExitError(command[0], result)
                })(),
          ...result,
        })
      })
    })
  }

  /**
   * @description run a command inside this subcontainer, throwing on non-zero exit status
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  async execFail(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
    abort?: AbortController,
  ): Promise<{
    stdout: string | Buffer
    stderr: string | Buffer
  }> {
    return this.exec(command, options, timeoutMs, abort).then((res) =>
      res.throw(),
    )
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

  rc(): SubContainerRc<Manifest, Effects> {
    return new SubContainerRc(this)
  }

  isOwned(): this is SubContainerOwned<Manifest, Effects> {
    return true
  }
}

export class SubContainerRc<
    Manifest extends T.SDKManifest,
    Effects extends T.Effects = T.Effects,
  >
  extends Drop
  implements SubContainer<Manifest, Effects>
{
  get imageId() {
    return this.subcontainer.imageId
  }
  get rootfs() {
    return this.subcontainer.rootfs
  }
  get guid() {
    return this.subcontainer.guid
  }
  private destroyed = false
  public constructor(
    private readonly subcontainer: SubContainerOwned<Manifest, Effects>,
  ) {
    subcontainer.rcs++
    super()
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
    return new SubContainerRc(
      await SubContainerOwned.of(effects, image, mounts, name),
    )
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
    const subContainer = await SubContainerRc.of(effects, image, mounts, name)
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
  ): Promise<this> {
    await this.subcontainer.mount(mounts)
    return this
  }

  get destroy() {
    return async () => {
      if (!this.destroyed) {
        const rcs = --this.subcontainer.rcs
        if (rcs <= 0) {
          await this.subcontainer.destroy()
          if (rcs < 0) console.error(new Error("UNREACHABLE: rcs < 0").stack)
        }
        this.destroyed = true
      }
      return null
    }
  }

  onDrop(): void {
    this.destroy()
  }

  /**
   * @description run a command inside this subcontainer
   * DOES NOT THROW ON NONZERO EXIT CODE (see execFail)
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  async exec(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
    abort?: AbortController,
  ): Promise<{
    throw: () => { stdout: string | Buffer; stderr: string | Buffer }
    exitCode: number | null
    exitSignal: NodeJS.Signals | null
    stdout: string | Buffer
    stderr: string | Buffer
  }> {
    return this.subcontainer.exec(command, options, timeoutMs, abort)
  }

  /**
   * @description run a command inside this subcontainer, throwing on non-zero exit status
   * @param commands an array representing the command and args to execute
   * @param options
   * @param timeoutMs how long to wait before killing the command in ms
   * @returns
   */
  async execFail(
    command: string[],
    options?: CommandOptions & ExecOptions,
    timeoutMs?: number | null,
    abort?: AbortController,
  ): Promise<{
    stdout: string | Buffer
    stderr: string | Buffer
  }> {
    return this.subcontainer.execFail(command, options, timeoutMs, abort)
  }

  async launch(
    command: string[],
    options?: CommandOptions,
  ): Promise<cp.ChildProcessWithoutNullStreams> {
    return this.subcontainer.launch(command, options)
  }

  async spawn(
    command: string[],
    options: CommandOptions & StdioOptions = { stdio: "inherit" },
  ): Promise<cp.ChildProcess> {
    return this.subcontainer.spawn(command, options)
  }

  rc(): SubContainerRc<Manifest, Effects> {
    return this.subcontainer.rc()
  }

  isOwned(): this is SubContainerOwned<Manifest, Effects> {
    return false
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
  volumeId: string
  subpath: string | null
  readonly: boolean
  filetype: "file" | "directory" | "infer"
}

export type MountOptionsAssets = {
  type: "assets"
  subpath: string | null
  filetype: "file" | "directory" | "infer"
}

export type MountOptionsPointer = {
  type: "pointer"
  packageId: string
  volumeId: string
  subpath: string | null
  readonly: boolean
  filetype: "file" | "directory" | "infer"
}

export type MountOptionsBackup = {
  type: "backup"
  subpath: string | null
  filetype: "file" | "directory" | "infer"
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
