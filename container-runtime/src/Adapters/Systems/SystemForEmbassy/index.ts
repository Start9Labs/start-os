import { types as T, utils, EmVer } from "@start9labs/start-sdk"
import * as fs from "fs/promises"

import { PolyfillEffects } from "./polyfillEffects"
import { Duration, duration } from "../../../Models/Duration"
import { System } from "../../../Interfaces/System"
import { matchManifest, Manifest, Procedure } from "./matchManifest"
import * as childProcess from "node:child_process"
import { DockerProcedureContainer } from "./DockerProcedureContainer"
import { promisify } from "node:util"
import * as U from "./oldEmbassyTypes"
import { MainLoop } from "./MainLoop"
import {
  matches,
  boolean,
  dictionary,
  literal,
  literals,
  object,
  string,
  unknown,
  any,
  tuple,
  number,
  anyOf,
  deferred,
  Parser,
  array,
} from "ts-matches"
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { JsonPath, unNestPath } from "../../../Models/JsonPath"
import { RpcResult, matchRpcResult } from "../../RpcListener"
import { CT } from "@start9labs/start-sdk"

type Optional<A> = A | undefined | null
function todo(): never {
  throw new Error("Not implemented")
}
const execFile = promisify(childProcess.execFile)

const MANIFEST_LOCATION = "/usr/lib/startos/package/embassyManifest.json"
const EMBASSY_JS_LOCATION = "/usr/lib/startos/package/embassy.js"
const EMBASSY_POINTER_PATH_PREFIX = "/embassyConfig"

const matchSetResult = object(
  {
    "depends-on": dictionary([string, array(string)]),
    dependsOn: dictionary([string, array(string)]),
    signal: literals(
      "SIGTERM",
      "SIGHUP",
      "SIGINT",
      "SIGQUIT",
      "SIGILL",
      "SIGTRAP",
      "SIGABRT",
      "SIGBUS",
      "SIGFPE",
      "SIGKILL",
      "SIGUSR1",
      "SIGSEGV",
      "SIGUSR2",
      "SIGPIPE",
      "SIGALRM",
      "SIGSTKFLT",
      "SIGCHLD",
      "SIGCONT",
      "SIGSTOP",
      "SIGTSTP",
      "SIGTTIN",
      "SIGTTOU",
      "SIGURG",
      "SIGXCPU",
      "SIGXFSZ",
      "SIGVTALRM",
      "SIGPROF",
      "SIGWINCH",
      "SIGIO",
      "SIGPWR",
      "SIGSYS",
      "SIGINFO",
    ),
  },
  ["depends-on", "dependsOn"],
)

export type PackagePropertiesV2 = {
  [name: string]: PackagePropertyObject | PackagePropertyString
}
export type PackagePropertyString = {
  type: "string"
  description?: string
  value: string
  /** Let's the ui make this copyable button */
  copyable?: boolean
  /** Let the ui create a qr for this field */
  qr?: boolean
  /** Hiding the value unless toggled off for field */
  masked?: boolean
}
export type PackagePropertyObject = {
  value: PackagePropertiesV2
  type: "object"
  description: string
}

const asProperty_ = (
  x: PackagePropertyString | PackagePropertyObject,
): T.PropertiesValue => {
  if (x.type === "object") {
    return {
      ...x,
      value: Object.fromEntries(
        Object.entries(x.value).map(([key, value]) => [
          key,
          asProperty_(value),
        ]),
      ),
    }
  }
  return {
    masked: false,
    description: null,
    qr: null,
    copyable: null,
    ...x,
  }
}
const asProperty = (x: PackagePropertiesV2): T.PropertiesReturn =>
  Object.fromEntries(
    Object.entries(x).map(([key, value]) => [key, asProperty_(value)]),
  )
const [matchPackageProperties, setMatchPackageProperties] =
  deferred<PackagePropertiesV2>()
const matchPackagePropertyObject: Parser<unknown, PackagePropertyObject> =
  object({
    value: matchPackageProperties,
    type: literal("object"),
    description: string,
  })

const matchPackagePropertyString: Parser<unknown, PackagePropertyString> =
  object(
    {
      type: literal("string"),
      description: string,
      value: string,
      copyable: boolean,
      qr: boolean,
      masked: boolean,
    },
    ["copyable", "description", "qr", "masked"],
  )
setMatchPackageProperties(
  dictionary([
    string,
    anyOf(matchPackagePropertyObject, matchPackagePropertyString),
  ]),
)

const matchProperties = object({
  version: literal(2),
  data: matchPackageProperties,
})

const DEFAULT_REGISTRY = "https://registry.start9.com"
export class SystemForEmbassy implements System {
  currentRunning: MainLoop | undefined
  static async of(manifestLocation: string = MANIFEST_LOCATION) {
    const moduleCode = await import(EMBASSY_JS_LOCATION)
      .catch((_) => require(EMBASSY_JS_LOCATION))
      .catch(async (_) => {
        console.error("Could not load the js")
        console.error({
          exists: await fs.stat(EMBASSY_JS_LOCATION),
        })
        return {}
      })
    const manifestData = await fs.readFile(manifestLocation, "utf-8")
    return new SystemForEmbassy(
      matchManifest.unsafeCast(JSON.parse(manifestData)),
      moduleCode,
    )
  }
  constructor(
    readonly manifest: Manifest,
    readonly moduleCode: Partial<U.ExpectedExports>,
  ) {}
  async execute(
    effects: HostSystemStartOs,
    options: {
      procedure: JsonPath
      input: unknown
      timeout?: number | undefined
    },
  ): Promise<RpcResult> {
    return this._execute(effects, options)
      .then((x) =>
        matches(x)
          .when(
            object({
              result: any,
            }),
            (x) => x,
          )
          .when(
            object({
              error: string,
            }),
            (x) => ({
              error: {
                code: 0,
                message: x.error,
              },
            }),
          )
          .when(
            object({
              "error-code": tuple(number, string),
            }),
            ({ "error-code": [code, message] }) => ({
              error: {
                code,
                message,
              },
            }),
          )
          .defaultTo({ result: x }),
      )
      .catch((error: unknown) => {
        if (error instanceof Error)
          return {
            error: {
              code: 0,
              message: error.name,
              data: {
                details: error.message,
                debug: `${error?.cause ?? "[noCause]"}:${error?.stack ?? "[noStack]"}`,
              },
            },
          }
        if (matchRpcResult.test(error)) return error
        return {
          error: {
            code: 0,
            message: String(error),
          },
        }
      })
  }
  async exit(effects: HostSystemStartOs): Promise<void> {
    if (this.currentRunning) await this.currentRunning.clean()
    delete this.currentRunning
  }
  async _execute(
    effects: HostSystemStartOs,
    options: {
      procedure: JsonPath
      input: unknown
      timeout?: number | undefined
    },
  ): Promise<unknown> {
    const input = options.input
    switch (options.procedure) {
      case "/backup/create":
        return this.createBackup(effects, options.timeout || null)
      case "/backup/restore":
        return this.restoreBackup(effects, options.timeout || null)
      case "/config/get":
        return this.getConfig(effects, options.timeout || null)
      case "/config/set":
        return this.setConfig(effects, input, options.timeout || null)
      case "/properties":
        return this.properties(effects, options.timeout || null)
      case "/actions/metadata":
        return todo()
      case "/init":
        return this.init(
          effects,
          string.optional().unsafeCast(input),
          options.timeout || null,
        )
      case "/uninit":
        return this.uninit(
          effects,
          string.optional().unsafeCast(input),
          options.timeout || null,
        )
      case "/main/start":
        return this.mainStart(effects, options.timeout || null)
      case "/main/stop":
        return this.mainStop(effects, options.timeout || null)
      default:
        const procedures = unNestPath(options.procedure)
        switch (true) {
          case procedures[1] === "actions" && procedures[3] === "get":
            return this.action(
              effects,
              procedures[2],
              input,
              options.timeout || null,
            )
          case procedures[1] === "actions" && procedures[3] === "run":
            return this.action(
              effects,
              procedures[2],
              input,
              options.timeout || null,
            )
          case procedures[1] === "dependencies" && procedures[3] === "query":
            return this.dependenciesAutoconfig(
              effects,
              procedures[2],
              input,
              options.timeout || null,
            )

          case procedures[1] === "dependencies" && procedures[3] === "update":
            return this.dependenciesAutoconfig(
              effects,
              procedures[2],
              input,
              options.timeout || null,
            )
        }
    }
    throw new Error(`Could not find the path for ${options.procedure}`)
  }
  private async init(
    effects: HostSystemStartOs,
    previousVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void> {
    if (previousVersion)
      await this.migration(effects, previousVersion, timeoutMs)
    await effects.setMainStatus({ status: "stopped" })
    await this.exportActions(effects)
    await this.exportNetwork(effects)
  }
  async exportNetwork(effects: HostSystemStartOs) {
    for (const [id, interfaceValue] of Object.entries(
      this.manifest.interfaces,
    )) {
      const hostId = `${id}-host`
      for (const [external, internal] of Object.entries(
        interfaceValue["tor-config"]?.["port-mapping"] ?? {},
      )) {
        const bindParams: T.BindParams = {
          kind: "multi",
          id: hostId,
          internalPort: Number.parseInt(internal),
          scheme: "http",
          preferredExternalPort: Number.parseInt(external),
          addSsl:
            external === "443"
              ? {
                  scheme: "https",
                  preferredExternalPort: Number.parseInt(external),
                  alpn: "reflect",
                }
              : null,
          secure:
            external === "443"
              ? {
                  ssl: true,
                }
              : null,
        }
        await effects.bind(bindParams)
      }
      for (const [external, value] of Object.entries(
        interfaceValue["lan-config"] ?? {},
      )) {
        const ssl = value.ssl
        const internal = value.internal
        const bindParams: T.BindParams = {
          kind: "multi",
          id: hostId,
          internalPort: internal,
          scheme: "http",
          preferredExternalPort: Number.parseInt(external),
          addSsl:
            external === "443"
              ? {
                  scheme: "https",
                  preferredExternalPort: Number.parseInt(external),
                  alpn: "reflect",
                }
              : null,
          secure: {
            ssl: true,
          },
        }
        await effects.bind(bindParams)
      }
      const options: T.ExportServiceInterfaceParams = {
        id,
        name: interfaceValue.name,
        description: interfaceValue.description,
        hasPrimary: id === "main" ? true : false,
        disabled: false,
        masked: false,
        addressInfo: {
          username: null,
          hostId: hostId,
          bindOptions: {
            scheme: "http",
            preferredExternalPort: 80,
            addSsl: null,
            secure: null,
          },
          suffix: "",
        },
        type: interfaceValue.ui ? "ui" : "api",
        hostKind: "multi",
        hostnames: [
          ...Object.entries(interfaceValue["lan-config"] || {}).map(
            ([key, value]): T.ExportedHostnameInfo => ({
              kind: "ip",
              networkInterfaceId: `${id}-lan-${key}`,
              public: true,
              hostname: {
                kind: "local",
                value: `${id}-lan-${key}-value`,
                port: value.internal,
                sslPort: null,
              },
            }),
          ),
          ...Object.entries(interfaceValue["tor-config"] || {}).map(
            ([key, value]): T.ExportedHostnameInfo =>
              ({
                kind: "onion",
                hostname: {
                  value: `${id}-lan-${key}-value`,
                  port: Number.parseInt(value.internal),
                  sslPort: null,
                },
              }) as const,
          ),
        ],
      }
      await effects.exportServiceInterface(options)
    }
  }
  async exportActions(effects: HostSystemStartOs) {
    const manifest = this.manifest
    if (!manifest.actions) return
    for (const [actionId, action] of Object.entries(manifest.actions)) {
      const hasRunning = !!action["allowed-statuses"].find(
        (x) => x === "running",
      )
      const hasStopped = !!action["allowed-statuses"].find(
        (x) => x === "stopped",
      )
      // prettier-ignore
      const allowedStatuses = hasRunning && hasStopped ? "any":
        hasRunning ? "onlyRunning" :
         "onlyStopped"
      await effects.exportAction({
        id: actionId,
        metadata: {
          name: action.name,
          description: action.description,
          warning: action.warning || null,
          input: action["input-spec"] as CT.InputSpec,
          disabled: false,
          allowedStatuses,
          group: null,
        },
      })
    }
  }
  private async uninit(
    effects: HostSystemStartOs,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void> {
    // TODO Do a migration down if the version exists
    await effects.setMainStatus({ status: "stopped" })
  }
  private async mainStart(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<void> {
    if (!!this.currentRunning) return

    this.currentRunning = new MainLoop(this, effects)
  }
  private async mainStop(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<Duration> {
    const { currentRunning } = this
    this.currentRunning?.clean()
    delete this.currentRunning
    if (currentRunning) {
      await currentRunning.clean({
        timeout: this.manifest.main["sigterm-timeout"],
      })
    }
    const durationValue = duration(this.manifest.main["sigterm-timeout"], "s")
    return durationValue
  }
  private async createBackup(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.of(effects, backup, {
        ...this.manifest.volumes,
        BACKUP: { type: "backup", readonly: false },
      })
      await container.execFail([backup.entrypoint, ...backup.args], timeoutMs)
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(
        new PolyfillEffects(effects, this.manifest),
      )
    }
  }
  private async restoreBackup(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        restoreBackup,
        {
          ...this.manifest.volumes,
          BACKUP: { type: "backup", readonly: true },
        },
      )
      await container.execFail(
        [restoreBackup.entrypoint, ...restoreBackup.args],
        timeoutMs,
      )
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(
        new PolyfillEffects(effects, this.manifest),
      )
    }
  }
  private async getConfig(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<T.ConfigRes> {
    return this.getConfigUncleaned(effects, timeoutMs).then(removePointers)
  }
  private async getConfigUncleaned(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<T.ConfigRes> {
    const config = this.manifest.config?.get
    if (!config) return { spec: {} }
    if (config.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        config,
        this.manifest.volumes,
      )
      // TODO: yaml
      return JSON.parse(
        (
          await container.execFail(
            [config.entrypoint, ...config.args],
            timeoutMs,
          )
        ).stdout.toString(),
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.getConfig
      if (!method) throw new Error("Expecting that the method getConfig exists")
      return (await method(new PolyfillEffects(effects, this.manifest)).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  private async setConfig(
    effects: HostSystemStartOs,
    newConfigWithoutPointers: unknown,
    timeoutMs: number | null,
  ): Promise<void> {
    const newConfig = structuredClone(newConfigWithoutPointers)
    await updateConfig(
      effects,
      await this.getConfigUncleaned(effects, timeoutMs).then((x) => x.spec),
      newConfig,
    )
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        setConfigValue,
        this.manifest.volumes,
      )
      const answer = matchSetResult.unsafeCast(
        JSON.parse(
          (
            await container.execFail(
              [
                setConfigValue.entrypoint,
                ...setConfigValue.args,
                JSON.stringify(newConfig),
              ],
              timeoutMs,
            )
          ).stdout.toString(),
        ),
      )
      const dependsOn = answer["depends-on"] ?? answer.dependsOn ?? {}
      await this.setConfigSetConfig(effects, dependsOn)
      return
    } else if (setConfigValue.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.setConfig
      if (!method) throw new Error("Expecting that the method setConfig exists")

      const answer = matchSetResult.unsafeCast(
        await method(
          new PolyfillEffects(effects, this.manifest),
          newConfig as U.Config,
        ).then((x): T.SetResult => {
          if ("result" in x)
            return {
              dependsOn: x.result["depends-on"],
              signal:
                x.result.signal === "SIGEMT" ? "SIGTERM" : x.result.signal,
            }
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        }),
      )
      const dependsOn = answer["depends-on"] ?? answer.dependsOn ?? {}
      await this.setConfigSetConfig(effects, dependsOn)
      return
    }
  }
  private async setConfigSetConfig(
    effects: HostSystemStartOs,
    dependsOn: { [x: string]: readonly string[] },
  ) {
    await effects.setDependencies({
      dependencies: Object.entries(dependsOn).flatMap(([key, value]) => {
        const dependency = this.manifest.dependencies?.[key]
        if (!dependency) return []
        const versionSpec = dependency.version
        const registryUrl = DEFAULT_REGISTRY
        const kind = "running"
        return [
          {
            id: key,
            versionSpec,
            registryUrl,
            kind,
            healthChecks: [...value],
          },
        ]
      }),
    })
  }

  private async migration(
    effects: HostSystemStartOs,
    fromVersion: string,
    timeoutMs: number | null,
  ): Promise<T.MigrationRes> {
    const fromEmver = EmVer.from(fromVersion)
    const currentEmver = EmVer.from(this.manifest.version)
    if (!this.manifest.migrations) return { configured: true }
    const fromMigration = Object.entries(this.manifest.migrations.from)
      .map(([version, procedure]) => [EmVer.from(version), procedure] as const)
      .find(
        ([versionEmver, procedure]) =>
          versionEmver.greaterThan(fromEmver) &&
          versionEmver.lessThanOrEqual(currentEmver),
      )
    const toMigration = Object.entries(this.manifest.migrations.to)
      .map(([version, procedure]) => [EmVer.from(version), procedure] as const)
      .find(
        ([versionEmver, procedure]) =>
          versionEmver.greaterThan(fromEmver) &&
          versionEmver.lessThanOrEqual(currentEmver),
      )

    // prettier-ignore
    const migration = (
        fromEmver.greaterThan(currentEmver) ? [toMigration, fromMigration] :
        [fromMigration, toMigration]).filter(Boolean)[0]

    if (migration) {
      const [version, procedure] = migration
      if (procedure.type === "docker") {
        const container = await DockerProcedureContainer.of(
          effects,
          procedure,
          this.manifest.volumes,
        )
        return JSON.parse(
          (
            await container.execFail(
              [
                procedure.entrypoint,
                ...procedure.args,
                JSON.stringify(fromVersion),
              ],
              timeoutMs,
            )
          ).stdout.toString(),
        )
      } else if (procedure.type === "script") {
        const moduleCode = await this.moduleCode
        const method = moduleCode.migration
        if (!method)
          throw new Error("Expecting that the method migration exists")
        return (await method(
          new PolyfillEffects(effects, this.manifest),
          fromVersion as string,
        ).then((x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        })) as any
      }
    }
    return { configured: true }
  }
  private async properties(
    effects: HostSystemStartOs,
    timeoutMs: number | null,
  ): Promise<ReturnType<T.ExpectedExports.Properties>> {
    // TODO BLU-J set the properties ever so often
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) throw new Error("There is no properties")
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        setConfigValue,
        this.manifest.volumes,
      )
      const properties = matchProperties.unsafeCast(
        JSON.parse(
          (
            await container.execFail(
              [setConfigValue.entrypoint, ...setConfigValue.args],
              timeoutMs,
            )
          ).stdout.toString(),
        ),
      )
      return asProperty(properties.data)
    } else if (setConfigValue.type === "script") {
      const moduleCode = this.moduleCode
      const method = moduleCode.properties
      if (!method)
        throw new Error("Expecting that the method properties exists")
      const properties = matchProperties.unsafeCast(
        await method(new PolyfillEffects(effects, this.manifest)).then((x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        }),
      )
      return asProperty(properties.data)
    }
    throw new Error(`Unknown type in the fetch properties: ${setConfigValue}`)
  }
  private async action(
    effects: HostSystemStartOs,
    actionId: string,
    formData: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions?.[actionId]?.implementation
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        actionProcedure,
        this.manifest.volumes,
      )
      return JSON.parse(
        (
          await container.execFail(
            [
              actionProcedure.entrypoint,
              ...actionProcedure.args,
              JSON.stringify(formData),
            ],
            timeoutMs,
          )
        ).stdout.toString(),
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.action?.[actionId]
      if (!method) throw new Error("Expecting that the method action exists")
      return (await method(
        new PolyfillEffects(effects, this.manifest),
        formData as any,
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })) as any
    }
  }
  private async dependenciesCheck(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies?.[id]?.config?.check
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        actionProcedure,
        this.manifest.volumes,
      )
      return JSON.parse(
        (
          await container.execFail(
            [
              actionProcedure.entrypoint,
              ...actionProcedure.args,
              JSON.stringify(oldConfig),
            ],
            timeoutMs,
          )
        ).stdout.toString(),
      )
    } else if (actionProcedure.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.dependencies?.[id]?.check
      if (!method)
        throw new Error(
          `Expecting that the method dependency check ${id} exists`,
        )
      return (await method(
        new PolyfillEffects(effects, this.manifest),
        oldConfig as any,
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })) as any
    } else {
      return {}
    }
  }
  private async dependenciesAutoconfig(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<void> {
    // TODO: docker
    const moduleCode = await this.moduleCode
    const method = moduleCode.dependencies?.[id]?.autoConfigure
    if (!method)
      throw new Error(
        `Expecting that the method dependency autoConfigure ${id} exists`,
      )
    return (await method(
      new PolyfillEffects(effects, this.manifest),
      oldConfig as any,
    ).then((x) => {
      if ("result" in x) return x.result
      if ("error" in x) throw new Error("Error getting config: " + x.error)
      throw new Error("Error getting config: " + x["error-code"][1])
    })) as any
  }
}
async function removePointers(value: T.ConfigRes): Promise<T.ConfigRes> {
  const startingSpec = structuredClone(value.spec)
  const config =
    value.config && cleanConfigFromPointers(value.config, startingSpec)
  const spec = cleanSpecOfPointers(startingSpec)

  return { config, spec }
}

const matchPointer = object({
  type: literal("pointer"),
})

const matchPointerPackage = object({
  subtype: literal("package"),
  target: literals("tor-key", "tor-address", "lan-address"),
  "package-id": string,
  interface: string,
})
const matchPointerConfig = object({
  subtype: literal("package"),
  target: literals("config"),
  "package-id": string,
  selector: string,
  multi: boolean,
})
const matchSpec = object({
  spec: object,
})
const matchVariants = object({ variants: dictionary([string, unknown]) })
function cleanSpecOfPointers<T>(mutSpec: T): T {
  if (!object.test(mutSpec)) return mutSpec
  for (const key in mutSpec) {
    const value = mutSpec[key]
    if (matchSpec.test(value)) value.spec = cleanSpecOfPointers(value.spec)
    if (matchVariants.test(value))
      value.variants = Object.fromEntries(
        Object.entries(value.variants).map(([key, value]) => [
          key,
          cleanSpecOfPointers(value),
        ]),
      )
    if (!matchPointer.test(value)) continue
    delete mutSpec[key]
    // // if (value.target === )
  }

  return mutSpec
}
function isKeyOf<O extends object>(
  key: string,
  ofObject: O,
): key is keyof O & string {
  return key in ofObject
}

// prettier-ignore
type CleanConfigFromPointers<C, S> = 
  [C, S] extends [object, object] ? {
    [K in (keyof C & keyof S ) & string]: (
      S[K] extends {type: "pointer"} ? never :
      S[K] extends {spec: object & infer B} ? CleanConfigFromPointers<C[K], B> :
      C[K]
    )
  } :
  null

function cleanConfigFromPointers<C, S>(
  config: C,
  spec: S,
): CleanConfigFromPointers<C, S> {
  const newConfig = {} as CleanConfigFromPointers<C, S>

  if (!(object.test(config) && object.test(spec)) || newConfig == null)
    return null as CleanConfigFromPointers<C, S>

  for (const key of Object.keys(spec)) {
    if (!isKeyOf(key, spec)) continue
    if (!isKeyOf(key, config)) continue
    const partSpec = spec[key]
    if (matchPointer.test(partSpec)) continue
    ;(newConfig as any)[key] = matchSpec.test(partSpec)
      ? cleanConfigFromPointers(config[key], partSpec.spec)
      : config[key]
  }
  return newConfig as CleanConfigFromPointers<C, S>
}

async function updateConfig(
  effects: HostSystemStartOs,
  spec: unknown,
  mutConfigValue: unknown,
) {
  if (!dictionary([string, unknown]).test(spec)) return
  if (!dictionary([string, unknown]).test(mutConfigValue)) return
  for (const key in spec) {
    const specValue = spec[key]

    const newConfigValue = mutConfigValue[key]
    if (matchSpec.test(specValue)) {
      const updateObject = { spec: null }
      await updateConfig(effects, { spec: specValue.spec }, updateObject)
      mutConfigValue[key] = updateObject.spec
    }
    if (
      matchVariants.test(specValue) &&
      object({ tag: object({ id: string }) }).test(newConfigValue) &&
      newConfigValue.tag.id in specValue.variants
    ) {
      // Not going to do anything on the variants...
    }
    if (!matchPointer.test(specValue)) continue
    if (matchPointerConfig.test(specValue)) {
      const configValue = (await effects.store.get({
        packageId: specValue["package-id"],
        callback() {},
        path: `${EMBASSY_POINTER_PATH_PREFIX}${specValue.selector}` as any,
      })) as any
      mutConfigValue[key] = configValue
    }
    if (matchPointerPackage.test(specValue)) {
      if (specValue.target === "tor-key")
        throw new Error("This service uses an unsupported target TorKey")
      const filled = await utils
        .getServiceInterface(effects, {
          packageId: specValue["package-id"],
          id: specValue.interface,
        })
        .once()
        .catch((x) => {
          console.error("Could not get the service interface", x)
          return null
        })

      mutConfigValue[key] =
        filled === null
          ? ""
          : specValue.target === "lan-address"
            ? filled.addressInfo.localHostnames[0] ||
              filled.addressInfo.onionHostnames[0]
            : filled.addressInfo.onionHostnames[0] ||
              filled.addressInfo.localHostnames[0]
    }
  }
}
