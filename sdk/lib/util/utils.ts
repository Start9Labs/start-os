import nullIfEmpty from "./nullIfEmpty"
import {
  CheckResult,
  checkPortListening,
  checkWebUrl,
} from "../health/checkFns"
import {
  DaemonReturned,
  Effects,
  EnsureStorePath,
  ExtractStore,
  InterfaceId,
  PackageId,
  ValidIfNoStupidEscape,
} from "../types"
import { GetSystemSmtp } from "./GetSystemSmtp"
import { DefaultString } from "../config/configTypes"
import { getDefaultString } from "./getDefaultString"
import { GetStore, getStore } from "../store/getStore"
import {
  MountDependenciesOut,
  mountDependencies,
} from "../dependency/mountDependencies"
import {
  ManifestId,
  VolumeName,
  NamedPath,
  Path,
} from "../dependency/setupDependencyMounts"
import { Host, MultiHost, SingleHost, StaticHost } from "../interfaces/Host"
import { NetworkInterfaceBuilder } from "../interfaces/NetworkInterfaceBuilder"
import { GetNetworkInterface, getNetworkInterface } from "./getNetworkInterface"
import {
  GetNetworkInterfaces,
  getNetworkInterfaces,
} from "./getNetworkInterfaces"
import * as CP from "node:child_process"
import { promisify } from "node:util"
import { splitCommand } from "./splitCommand"
import { SDKManifest } from "../manifest/ManifestTypes"
import { MountOptions, Overlay, CommandOptions } from "./Overlay"
export type Signals = NodeJS.Signals

export const SIGTERM: Signals = "SIGTERM"
export const SIGKILL: Signals = "SIGTERM"
export const NO_TIMEOUT = -1

const childProcess = {
  exec: promisify(CP.exec),
  execFile: promisify(CP.execFile),
}

export type NetworkInterfaceType = "ui" | "p2p" | "api" | "other"

export type Utils<
  Manifest extends SDKManifest,
  Store,
  WrapperOverWrite = { const: never },
> = {
  checkPortListening(
    port: number,
    options: {
      errorMessage: string
      successMessage: string
      timeoutMessage?: string
      timeout?: number
    },
  ): Promise<CheckResult>
  checkWebUrl(
    url: string,
    options?: {
      timeout?: number
      successMessage?: string
      errorMessage?: string
    },
  ): Promise<CheckResult>
  childProcess: typeof childProcess
  createInterface: (options: {
    name: string
    id: string
    description: string
    hasPrimary: boolean
    disabled: boolean
    type: NetworkInterfaceType
    username: null | string
    path: string
    search: Record<string, string>
  }) => NetworkInterfaceBuilder
  getSystemSmtp: () => GetSystemSmtp & WrapperOverWrite
  host: {
    static: (id: string) => StaticHost
    single: (id: string) => SingleHost
    multi: (id: string) => MultiHost
  }
  mountDependencies: <
    In extends
      | Record<ManifestId, Record<VolumeName, Record<NamedPath, Path>>>
      | Record<VolumeName, Record<NamedPath, Path>>
      | Record<NamedPath, Path>
      | Path,
  >(
    value: In,
  ) => Promise<MountDependenciesOut<In>>
  networkInterface: {
    getOwn: (interfaceId: InterfaceId) => GetNetworkInterface & WrapperOverWrite
    get: (opts: {
      interfaceId: InterfaceId
      packageId: PackageId
    }) => GetNetworkInterface & WrapperOverWrite
    getAllOwn: () => GetNetworkInterfaces & WrapperOverWrite
    getAll: (opts: {
      packageId: PackageId
    }) => GetNetworkInterfaces & WrapperOverWrite
  }
  nullIfEmpty: typeof nullIfEmpty
  runCommand: <A extends string>(
    imageId: Manifest["images"][number],
    command: ValidIfNoStupidEscape<A> | [string, ...string[]],
    options: CommandOptions & {
      mounts?: { path: string; options: MountOptions }[]
    },
  ) => Promise<{ stdout: string | Buffer; stderr: string | Buffer }>
  runDaemon: <A extends string>(
    imageId: Manifest["images"][number],
    command: ValidIfNoStupidEscape<A> | [string, ...string[]],
    options: CommandOptions & {
      mounts?: { path: string; options: MountOptions }[]
      overlay?: Overlay
    },
  ) => Promise<DaemonReturned>
  store: {
    get: <Path extends string>(
      packageId: string,
      path: EnsureStorePath<Store, Path>,
    ) => GetStore<Store, Path> & WrapperOverWrite
    getOwn: <Path extends string>(
      path: EnsureStorePath<Store, Path>,
    ) => GetStore<Store, Path> & WrapperOverWrite
    setOwn: <Path extends string | never>(
      path: EnsureStorePath<Store, Path>,
      value: ExtractStore<Store, Path>,
    ) => Promise<void>
  }
}
export const createUtils = <
  Manifest extends SDKManifest,
  Store = never,
  WrapperOverWrite = { const: never },
>(
  effects: Effects,
): Utils<Manifest, Store, WrapperOverWrite> => {
  return {
    createInterface: (options: {
      name: string
      id: string
      description: string
      hasPrimary: boolean
      disabled: boolean
      type: NetworkInterfaceType
      username: null | string
      path: string
      search: Record<string, string>
    }) => new NetworkInterfaceBuilder({ ...options, effects }),
    childProcess,
    getSystemSmtp: () =>
      new GetSystemSmtp(effects) as GetSystemSmtp & WrapperOverWrite,

    host: {
      static: (id: string) => new StaticHost({ id, effects }),
      single: (id: string) => new SingleHost({ id, effects }),
      multi: (id: string) => new MultiHost({ id, effects }),
    },
    nullIfEmpty,

    networkInterface: {
      getOwn: (interfaceId: InterfaceId) =>
        getNetworkInterface(effects, { interfaceId }) as GetNetworkInterface &
          WrapperOverWrite,
      get: (opts: { interfaceId: InterfaceId; packageId: PackageId }) =>
        getNetworkInterface(effects, opts) as GetNetworkInterface &
          WrapperOverWrite,
      getAllOwn: () =>
        getNetworkInterfaces(effects, {}) as GetNetworkInterfaces &
          WrapperOverWrite,
      getAll: (opts: { packageId: PackageId }) =>
        getNetworkInterfaces(effects, opts) as GetNetworkInterfaces &
          WrapperOverWrite,
    },
    store: {
      get: <Path extends string = never>(
        packageId: string,
        path: EnsureStorePath<Store, Path>,
      ) =>
        getStore<Store, Path>(effects, path as any, {
          packageId,
        }) as any,
      getOwn: <Path extends string>(path: EnsureStorePath<Store, Path>) =>
        getStore<Store, Path>(effects, path as any) as any,
      setOwn: <Path extends string | never>(
        path: EnsureStorePath<Store, Path>,
        value: ExtractStore<Store, Path>,
      ) => effects.store.set<Store, Path>({ value, path: path as any }),
    },

    runCommand: async <A extends string>(
      imageId: Manifest["images"][number],
      command: ValidIfNoStupidEscape<A> | [string, ...string[]],
      options: CommandOptions & {
        mounts?: { path: string; options: MountOptions }[]
      },
    ): Promise<{ stdout: string | Buffer; stderr: string | Buffer }> => {
      const commands = splitCommand(command)
      const overlay = await Overlay.of(effects, imageId)
      try {
        for (let mount of options.mounts || []) {
          await overlay.mount(mount.options, mount.path)
        }
        return await overlay.exec(commands)
      } finally {
        await overlay.destroy()
      }
    },
    runDaemon: async <A extends string>(
      imageId: Manifest["images"][number],
      command: ValidIfNoStupidEscape<A> | [string, ...string[]],
      options: CommandOptions & {
        mounts?: { path: string; options: MountOptions }[]
        overlay?: Overlay
      },
    ): Promise<DaemonReturned> => {
      const commands = splitCommand(command)
      const overlay = options.overlay || (await Overlay.of(effects, imageId))
      for (let mount of options.mounts || []) {
        await overlay.mount(mount.options, mount.path)
      }
      const childProcess = overlay.spawn(commands, {
        env: options.env,
      })
      const answer = new Promise<null>((resolve, reject) => {
        childProcess.stdout.on("data", (data: any) => {
          console.log(data.toString())
        })
        childProcess.stderr.on("data", (data: any) => {
          console.error(data.toString())
        })

        childProcess.on("close", (code: any) => {
          if (code === 0) {
            return resolve(null)
          }
          return reject(new Error(`${commands[0]} exited with code ${code}`))
        })
      })

      return {
        wait() {
          return answer
        },
        async term({ signal = SIGTERM, timeout = NO_TIMEOUT } = {}) {
          try {
            childProcess.kill(signal)

            if (timeout <= NO_TIMEOUT) {
              const didTimeout = await Promise.race([
                new Promise((resolve) => setTimeout(resolve, timeout)).then(
                  () => true,
                ),
                answer.then(() => false),
              ])
              if (didTimeout) childProcess.kill(SIGKILL)
            }
            await answer
          } finally {
            await overlay.destroy()
          }
        },
      }
    },
    checkPortListening: checkPortListening.bind(null, effects),
    checkWebUrl: checkWebUrl.bind(null, effects),

    mountDependencies: <
      In extends
        | Record<ManifestId, Record<VolumeName, Record<NamedPath, Path>>>
        | Record<VolumeName, Record<NamedPath, Path>>
        | Record<NamedPath, Path>
        | Path,
    >(
      value: In,
    ) => mountDependencies(effects, value),
  }
}
function noop(): void {}
