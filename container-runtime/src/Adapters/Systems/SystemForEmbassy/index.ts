import { ExtendedVersion, types as T, utils } from "@start9labs/start-sdk"
import * as fs from "fs/promises"

import { polyfillEffects } from "./polyfillEffects"
import { fromDuration } from "../../../Models/Duration"
import { System } from "../../../Interfaces/System"
import { matchManifest, Manifest } from "./matchManifest"
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
import { AddSslOptions } from "@start9labs/start-sdk/base/lib/osBindings"
import {
  BindOptionsByProtocol,
  MultiHost,
} from "@start9labs/start-sdk/base/lib/interfaces/Host"
import { ServiceInterfaceBuilder } from "@start9labs/start-sdk/base/lib/interfaces/ServiceInterfaceBuilder"
import { Effects } from "../../../Models/Effects"
import {
  OldConfigSpec,
  matchOldConfigSpec,
  transformConfigSpec,
  transformNewConfigToOld,
  transformOldConfigToNew,
} from "./transformConfigSpec"
import { partialDiff } from "@start9labs/start-sdk/base/lib/util"

type Optional<A> = A | undefined | null
function todo(): never {
  throw new Error("Not implemented")
}

const MANIFEST_LOCATION = "/usr/lib/startos/package/embassyManifest.json"
export const EMBASSY_JS_LOCATION = "/usr/lib/startos/package/embassy.js"
const EMBASSY_POINTER_PATH_PREFIX = "/embassyConfig" as utils.StorePath

const matchResult = object({
  result: any,
})
const matchError = object({
  error: string,
})
const matchErrorCode = object<{
  "error-code": [number, string] | readonly [number, string]
}>({
  "error-code": tuple(number, string),
})

const assertNever = (
  x: never,
  message = "Not expecting to get here: ",
): never => {
  throw new Error(message + JSON.stringify(x))
}
/**
  Should be changing the type for specific properties, and this is mostly a transformation for the old return types to the newer one.
*/
const fromReturnType = <A>(a: U.ResultType<A>): A => {
  if (matchResult.test(a)) {
    return a.result
  }
  if (matchError.test(a)) {
    console.info({ passedErrorStack: new Error().stack, error: a.error })
    throw { error: a.error }
  }
  if (matchErrorCode.test(a)) {
    const [code, message] = a["error-code"]
    throw { error: message, code }
  }
  return assertNever(a)
}

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

type OldGetConfigRes = {
  config?: null | Record<string, unknown>
  spec: OldConfigSpec
}

export type PropertiesValue =
  | {
      /** The type of this value, either "string" or "object" */
      type: "object"
      /** A nested mapping of values. The user will experience this as a nested page with back button */
      value: { [k: string]: PropertiesValue }
      /** (optional) A human readable description of the new set of values */
      description: string | null
    }
  | {
      /** The type of this value, either "string" or "object" */
      type: "string"
      /** The value to display to the user */
      value: string
      /** A human readable description of the value */
      description: string | null
      /** Whether or not to mask the value, for example, when displaying a password */
      masked: boolean | null
      /** Whether or not to include a button for copying the value to clipboard */
      copyable: boolean | null
      /** Whether or not to include a button for displaying the value as a QR code */
      qr: boolean | null
    }

export type PropertiesReturn = {
  [key: string]: PropertiesValue
}

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
): PropertiesValue => {
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
const asProperty = (x: PackagePropertiesV2): PropertiesReturn =>
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

function convertProperties(
  name: string,
  value: PropertiesValue,
): T.ActionResultMember {
  if (value.type === "string") {
    return {
      type: "single",
      name,
      description: value.description,
      copyable: value.copyable || false,
      masked: value.masked || false,
      qr: value.qr || false,
      value: value.value,
    }
  }
  return {
    type: "group",
    name,
    description: value.description,
    value: Object.entries(value.value).map(([name, value]) =>
      convertProperties(name, value),
    ),
  }
}

const DEFAULT_REGISTRY = "https://registry.start9.com"
export class SystemForEmbassy implements System {
  currentRunning: MainLoop | undefined
  static async of(manifestLocation: string = MANIFEST_LOCATION) {
    const moduleCode = await import(EMBASSY_JS_LOCATION)
      .catch((_) => require(EMBASSY_JS_LOCATION))
      .catch(async (_) => {
        console.error(utils.asError("Could not load the js"))
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

  async containerInit(effects: Effects): Promise<void> {
    for (let depId in this.manifest.dependencies) {
      if (this.manifest.dependencies[depId].config) {
        await this.dependenciesAutoconfig(effects, depId, null)
      }
    }
    await effects.setMainStatus({ status: "stopped" })
    await this.exportActions(effects)
    await this.exportNetwork(effects)
    await this.containerSetDependencies(effects)
  }
  async containerSetDependencies(effects: T.Effects) {
    const oldDeps: Record<string, string[]> = Object.fromEntries(
      await effects
        .getDependencies()
        .then((x) =>
          x.flatMap((x) =>
            x.kind === "running" ? [[x.id, x?.healthChecks || []]] : [],
          ),
        )
        .catch(() => []),
    )
    await this.setDependencies(effects, oldDeps)
  }

  async exit(): Promise<void> {
    if (this.currentRunning) await this.currentRunning.clean()
    delete this.currentRunning
  }

  async start(effects: T.Effects): Promise<void> {
    effects.constRetry = utils.once(() => effects.restart())
    if (!!this.currentRunning) return

    this.currentRunning = await MainLoop.of(this, effects)
  }
  callCallback(_callback: number, _args: any[]): void {}
  async stop(): Promise<void> {
    const { currentRunning } = this
    this.currentRunning?.clean()
    delete this.currentRunning
    if (currentRunning) {
      await currentRunning.clean({
        timeout: fromDuration(this.manifest.main["sigterm-timeout"] || "30s"),
      })
    }
  }

  async packageInit(effects: Effects, timeoutMs: number | null): Promise<void> {
    const previousVersion = await effects.getDataVersion()
    if (previousVersion) {
      if (
        (await this.migration(effects, previousVersion, timeoutMs)).configured
      ) {
        await effects.action.clearRequests({ only: ["needs-config"] })
      }
      await effects.setDataVersion({
        version: ExtendedVersion.parseEmver(this.manifest.version).toString(),
      })
    } else if (this.manifest.config) {
      await effects.action.request({
        packageId: this.manifest.id,
        actionId: "config",
        severity: "critical",
        replayId: "needs-config",
        reason: "This service must be configured before it can be run",
      })
    }
  }
  async exportNetwork(effects: Effects) {
    for (const [id, interfaceValue] of Object.entries(
      this.manifest.interfaces,
    )) {
      const host = new MultiHost({ effects, id })
      const internalPorts = new Set(
        Object.values(interfaceValue["tor-config"]?.["port-mapping"] ?? {})
          .map(Number.parseInt)
          .concat(
            ...Object.values(interfaceValue["lan-config"] ?? {}).map(
              (c) => c.internal,
            ),
          )
          .filter(Boolean),
      )
      const bindings = Array.from(internalPorts).map<
        [number, BindOptionsByProtocol]
      >((port) => {
        const lanPort = Object.entries(interfaceValue["lan-config"] ?? {}).find(
          ([external, internal]) => internal.internal === port,
        )?.[0]
        const torPort = Object.entries(
          interfaceValue["tor-config"]?.["port-mapping"] ?? {},
        ).find(
          ([external, internal]) => Number.parseInt(internal) === port,
        )?.[0]
        let addSsl: AddSslOptions | null = null
        if (lanPort) {
          const lanPortNum = Number.parseInt(lanPort)
          if (lanPortNum === 443) {
            return [port, { protocol: "http", preferredExternalPort: 80 }]
          }
          addSsl = {
            preferredExternalPort: lanPortNum,
            alpn: { specified: [] },
          }
        }
        return [
          port,
          {
            secure: null,
            preferredExternalPort: Number.parseInt(
              torPort || lanPort || String(port),
            ),
            addSsl,
          },
        ]
      })

      await Promise.all(
        bindings.map(async ([internal, options]) => {
          if (internal == null) {
            return
          }
          if (options?.preferredExternalPort == null) {
            return
          }
          const origin = await host.bindPort(internal, options)
          await origin.export([
            new ServiceInterfaceBuilder({
              effects,
              name: interfaceValue.name,
              id: `${id}-${internal}`,
              description: interfaceValue.description,
              hasPrimary: false,
              type:
                interfaceValue.ui &&
                (origin.scheme === "http" || origin.sslScheme === "https")
                  ? "ui"
                  : "api",
              masked: false,
              path: "",
              schemeOverride: null,
              search: {},
              username: null,
            }),
          ])
        }),
      )
    }
  }
  async getActionInput(
    effects: Effects,
    actionId: string,
    timeoutMs: number | null,
  ): Promise<T.ActionInput | null> {
    if (actionId === "config") {
      const config = await this.getConfig(effects, timeoutMs)
      return { spec: config.spec, value: config.config }
    } else if (actionId === "properties") {
      return null
    } else {
      const oldSpec = this.manifest.actions?.[actionId]?.["input-spec"]
      if (!oldSpec) return null
      return {
        spec: transformConfigSpec(oldSpec as OldConfigSpec),
        value: null,
      }
    }
  }
  async runAction(
    effects: Effects,
    actionId: string,
    input: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult | null> {
    if (actionId === "config") {
      await this.setConfig(effects, input, timeoutMs)
      return null
    } else if (actionId === "properties") {
      return {
        version: "1",
        title: "Properties",
        message: null,
        result: {
          type: "group",
          value: Object.entries(await this.properties(effects, timeoutMs)).map(
            ([name, value]) => convertProperties(name, value),
          ),
        },
      }
    } else {
      return this.action(effects, actionId, input, timeoutMs)
    }
  }
  async exportActions(effects: Effects) {
    const manifest = this.manifest
    const actions = {
      ...manifest.actions,
    }
    if (manifest.config) {
      actions.config = {
        name: "Configure",
        description: `Customize ${manifest.title}`,
        "allowed-statuses": ["running", "stopped"],
        "input-spec": {},
        implementation: { type: "script", args: [] },
      }
    }
    if (manifest.properties) {
      actions.properties = {
        name: "Properties",
        description:
          "Runtime information, credentials, and other values of interest",
        "allowed-statuses": ["running", "stopped"],
        "input-spec": null,
        implementation: { type: "script", args: [] },
      }
    }
    for (const [actionId, action] of Object.entries(actions)) {
      const hasRunning = !!action["allowed-statuses"].find(
        (x) => x === "running",
      )
      const hasStopped = !!action["allowed-statuses"].find(
        (x) => x === "stopped",
      )
      // prettier-ignore
      const allowedStatuses = hasRunning && hasStopped ? "any":
        hasRunning ? "only-running" :
         "only-stopped"
      await effects.action.export({
        id: actionId,
        metadata: {
          name: action.name,
          description: action.description,
          warning: action.warning || null,
          visibility: "enabled",
          allowedStatuses,
          hasInput: !!action["input-spec"],
          group: null,
        },
      })
    }
    await effects.action.clear({ except: Object.keys(actions) })
  }
  async packageUninit(
    effects: Effects,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void> {
    // TODO Do a migration down if the version exists
    await effects.setMainStatus({ status: "stopped" })
  }

  async createBackup(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const commands = [backup.entrypoint, ...backup.args]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        backup,
        {
          ...this.manifest.volumes,
          BACKUP: { type: "backup", readonly: false },
        },
        `Backup - ${commands.join(" ")}`,
      )
      await container.execFail(commands, timeoutMs)
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(polyfillEffects(effects, this.manifest))
    }
  }
  async restoreBackup(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const commands = [restoreBackup.entrypoint, ...restoreBackup.args]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        restoreBackup,
        {
          ...this.manifest.volumes,
          BACKUP: { type: "backup", readonly: true },
        },
        `Restore Backup - ${commands.join(" ")}`,
      )
      await container.execFail(commands, timeoutMs)
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(polyfillEffects(effects, this.manifest))
    }
  }
  async getConfig(effects: Effects, timeoutMs: number | null) {
    return this.getConfigUncleaned(effects, timeoutMs).then(convertToNewConfig)
  }
  private async getConfigUncleaned(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<OldGetConfigRes> {
    const config = this.manifest.config?.get
    if (!config) return { spec: {} }
    if (config.type === "docker") {
      const commands = [config.entrypoint, ...config.args]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        config,
        this.manifest.volumes,
        `Get Config - ${commands.join(" ")}`,
      )
      // TODO: yaml
      return JSON.parse(
        (await container.execFail(commands, timeoutMs)).stdout.toString(),
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.getConfig
      if (!method) throw new Error("Expecting that the method getConfig exists")
      return (await method(polyfillEffects(effects, this.manifest)).then(
        (x) => {
          if ("result" in x) return JSON.parse(JSON.stringify(x.result))
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  async setConfig(
    effects: Effects,
    newConfigWithoutPointers: unknown,
    timeoutMs: number | null,
  ): Promise<void> {
    const spec = await this.getConfigUncleaned(effects, timeoutMs).then(
      (x) => x.spec,
    )
    const newConfig = transformNewConfigToOld(
      spec,
      structuredClone(newConfigWithoutPointers as Record<string, unknown>),
    )
    await updateConfig(effects, this.manifest, spec, newConfig)
    await effects.store.set({
      path: EMBASSY_POINTER_PATH_PREFIX,
      value: newConfig,
    })
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return
    if (setConfigValue.type === "docker") {
      const commands = [
        setConfigValue.entrypoint,
        ...setConfigValue.args,
        JSON.stringify(newConfig),
      ]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        setConfigValue,
        this.manifest.volumes,
        `Set Config - ${commands.join(" ")}`,
      )
      const answer = matchSetResult.unsafeCast(
        JSON.parse(
          (await container.execFail(commands, timeoutMs)).stdout.toString(),
        ),
      )
      const dependsOn = answer["depends-on"] ?? answer.dependsOn ?? {}
      await this.setDependencies(effects, dependsOn)
      return
    } else if (setConfigValue.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.setConfig
      if (!method) throw new Error("Expecting that the method setConfig exists")

      const answer = matchSetResult.unsafeCast(
        await method(
          polyfillEffects(effects, this.manifest),
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
      await this.setDependencies(effects, dependsOn)
      return
    }
  }
  private async setDependencies(
    effects: Effects,
    rawDepends: { [x: string]: readonly string[] },
  ) {
    const dependsOn: Record<string, readonly string[] | null> = {
      ...Object.fromEntries(
        Object.entries(this.manifest.dependencies || {})?.map((x) => [
          x[0],
          null,
        ]) || [],
      ),
      ...rawDepends,
    }
    await effects.setDependencies({
      dependencies: Object.entries(dependsOn).flatMap(
        ([key, value]): T.Dependencies => {
          const dependency = this.manifest.dependencies?.[key]
          if (!dependency) return []
          if (value == null) {
            const versionRange = dependency.version
            if (dependency.requirement.type === "required") {
              return [
                {
                  id: key,
                  versionRange,
                  kind: "running",
                  healthChecks: [],
                },
              ]
            }
            return [
              {
                kind: "exists",
                id: key,
                versionRange,
              },
            ]
          }
          const versionRange = dependency.version
          const kind = "running"
          return [
            {
              id: key,
              versionRange,
              kind,
              healthChecks: [...value],
            },
          ]
        },
      ),
    })
  }

  async migration(
    effects: Effects,
    fromVersion: string,
    timeoutMs: number | null,
  ): Promise<{ configured: boolean }> {
    const fromEmver = ExtendedVersion.parseEmver(fromVersion)
    const currentEmver = ExtendedVersion.parseEmver(this.manifest.version)
    if (!this.manifest.migrations) return { configured: true }
    const fromMigration = Object.entries(this.manifest.migrations.from)
      .map(
        ([version, procedure]) =>
          [ExtendedVersion.parseEmver(version), procedure] as const,
      )
      .find(
        ([versionEmver, procedure]) =>
          versionEmver.greaterThan(fromEmver) &&
          versionEmver.lessThanOrEqual(currentEmver),
      )
    const toMigration = Object.entries(this.manifest.migrations.to)
      .map(
        ([version, procedure]) =>
          [ExtendedVersion.parseEmver(version), procedure] as const,
      )
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
        const commands = [
          procedure.entrypoint,
          ...procedure.args,
          JSON.stringify(fromVersion),
        ]
        const container = await DockerProcedureContainer.of(
          effects,
          this.manifest.id,
          procedure,
          this.manifest.volumes,
          `Migration - ${commands.join(" ")}`,
        )
        return JSON.parse(
          (await container.execFail(commands, timeoutMs)).stdout.toString(),
        )
      } else if (procedure.type === "script") {
        const moduleCode = await this.moduleCode
        const method = moduleCode.migration
        if (!method)
          throw new Error("Expecting that the method migration exists")
        return (await method(
          polyfillEffects(effects, this.manifest),
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
  async properties(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<PropertiesReturn> {
    // TODO BLU-J set the properties ever so often
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) throw new Error("There is no properties")
    if (setConfigValue.type === "docker") {
      const commands = [setConfigValue.entrypoint, ...setConfigValue.args]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        setConfigValue,
        this.manifest.volumes,
        `Properties - ${commands.join(" ")}`,
      )
      const properties = matchProperties.unsafeCast(
        JSON.parse(
          (await container.execFail(commands, timeoutMs)).stdout.toString(),
        ),
      )
      return asProperty(properties.data)
    } else if (setConfigValue.type === "script") {
      const moduleCode = this.moduleCode
      const method = moduleCode.properties
      if (!method)
        throw new Error("Expecting that the method properties exists")
      const properties = matchProperties.unsafeCast(
        await method(polyfillEffects(effects, this.manifest)).then(
          fromReturnType,
        ),
      )
      return asProperty(properties.data)
    }
    throw new Error(`Unknown type in the fetch properties: ${setConfigValue}`)
  }
  async action(
    effects: Effects,
    actionId: string,
    formData: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions?.[actionId]?.implementation
    const toActionResult = ({
      message,
      value,
      copyable,
      qr,
    }: U.ActionResult): T.ActionResult => ({
      version: "0",
      message,
      value: value ?? null,
      copyable,
      qr,
    })
    if (!actionProcedure) throw Error("Action not found")
    if (actionProcedure.type === "docker") {
      const subcontainer = actionProcedure.inject
        ? this.currentRunning?.mainSubContainerHandle
        : undefined

      const env: Record<string, string> = actionProcedure.inject
        ? {
            HOME: "/root",
          }
        : {}
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        actionProcedure,
        this.manifest.volumes,
        `Action ${actionId}`,
        {
          subcontainer,
        },
      )
      return toActionResult(
        JSON.parse(
          (
            await container.execFail(
              [
                actionProcedure.entrypoint,
                ...actionProcedure.args,
                JSON.stringify(formData),
              ],
              timeoutMs,
              { env },
            )
          ).stdout.toString(),
        ),
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.action?.[actionId]
      if (!method) throw new Error("Expecting that the method action exists")
      return await method(
        polyfillEffects(effects, this.manifest),
        formData as any,
      )
        .then(fromReturnType)
        .then(toActionResult)
    }
  }
  async dependenciesCheck(
    effects: Effects,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies?.[id]?.config?.check
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const commands = [
        actionProcedure.entrypoint,
        ...actionProcedure.args,
        JSON.stringify(oldConfig),
      ]
      const container = await DockerProcedureContainer.of(
        effects,
        this.manifest.id,
        actionProcedure,
        this.manifest.volumes,
        `Dependencies Check - ${commands.join(" ")}`,
      )
      return JSON.parse(
        (await container.execFail(commands, timeoutMs)).stdout.toString(),
      )
    } else if (actionProcedure.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.dependencies?.[id]?.check
      if (!method)
        throw new Error(
          `Expecting that the method dependency check ${id} exists`,
        )
      return (await method(
        polyfillEffects(effects, this.manifest),
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
  async dependenciesAutoconfig(
    effects: Effects,
    id: string,
    timeoutMs: number | null,
  ): Promise<void> {
    // TODO: docker
    const oldConfig = (await effects.store.get({
      packageId: id,
      path: EMBASSY_POINTER_PATH_PREFIX,
      callback: () => {
        this.dependenciesAutoconfig(effects, id, timeoutMs)
      },
    })) as U.Config
    if (!oldConfig) return
    const moduleCode = await this.moduleCode
    const method = moduleCode.dependencies?.[id]?.autoConfigure
    if (!method) return
    const newConfig = (await method(
      polyfillEffects(effects, this.manifest),
      JSON.parse(JSON.stringify(oldConfig)),
    ).then((x) => {
      if ("result" in x) return x.result
      if ("error" in x) throw new Error("Error getting config: " + x.error)
      throw new Error("Error getting config: " + x["error-code"][1])
    })) as any
    const diff = partialDiff(oldConfig, newConfig)
    if (diff) {
      await effects.action.request({
        actionId: "config",
        packageId: id,
        replayId: `${id}/config`,
        severity: "important",
        reason: `Configure this dependency for the needs of ${this.manifest.title}`,
        input: {
          kind: "partial",
          value: diff.diff,
        },
        when: {
          condition: "input-not-matches",
          once: false,
        },
      })
    }
  }
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

async function updateConfig(
  effects: Effects,
  manifest: Manifest,
  spec: OldConfigSpec,
  mutConfigValue: Record<string, unknown>,
) {
  for (const key in spec) {
    const specValue = spec[key]

    if (specValue.type === "object") {
      await updateConfig(
        effects,
        manifest,
        specValue.spec as OldConfigSpec,
        mutConfigValue[key] as Record<string, unknown>,
      )
    } else if (specValue.type === "list" && specValue.subtype === "object") {
      const list = mutConfigValue[key] as unknown[]
      for (let val of list) {
        await updateConfig(
          effects,
          manifest,
          { ...(specValue.spec as any), type: "object" as const },
          val as Record<string, unknown>,
        )
      }
    } else if (specValue.type === "union") {
      const union = mutConfigValue[key] as Record<string, unknown>
      await updateConfig(
        effects,
        manifest,
        specValue.variants[union[specValue.tag.id] as string] as OldConfigSpec,
        mutConfigValue[key] as Record<string, unknown>,
      )
    } else if (
      specValue.type === "pointer" &&
      specValue.subtype === "package"
    ) {
      if (specValue.target === "config") {
        const jp = require("jsonpath")
        const remoteConfig = await effects.store.get({
          packageId: specValue["package-id"],
          callback: () => effects.restart(),
          path: EMBASSY_POINTER_PATH_PREFIX,
        })
        console.debug(remoteConfig)
        const configValue = specValue.multi
          ? jp.query(remoteConfig, specValue.selector)
          : jp.query(remoteConfig, specValue.selector, 1)[0]
        mutConfigValue[key] = configValue === undefined ? null : configValue
      } else if (specValue.target === "tor-key") {
        throw new Error("This service uses an unsupported target TorKey")
      } else {
        const specInterface = specValue.interface
        const serviceInterfaceId = extractServiceInterfaceId(
          manifest,
          specInterface,
        )
        if (!serviceInterfaceId) {
          mutConfigValue[key] = ""
          return
        }
        const filled = await utils
          .getServiceInterface(effects, {
            packageId: specValue["package-id"],
            id: serviceInterfaceId,
          })
          .once()
          .catch((x) => {
            console.error(
              "Could not get the service interface",
              utils.asError(x),
            )
            return null
          })
        const catchFn = <X>(fn: () => X) => {
          try {
            return fn()
          } catch (e) {
            return undefined
          }
        }
        const url: string =
          filled === null || filled.addressInfo === null
            ? ""
            : catchFn(() =>
                utils.hostnameInfoToAddress(
                  specValue.target === "lan-address"
                    ? filled.addressInfo!.localHostnames[0] ||
                        filled.addressInfo!.onionHostnames[0]
                    : filled.addressInfo!.onionHostnames[0] ||
                        filled.addressInfo!.localHostnames[0],
                ),
              ) || ""
        mutConfigValue[key] = url
      }
    }
  }
}
function extractServiceInterfaceId(manifest: Manifest, specInterface: string) {
  const internalPort =
    Object.entries(
      manifest.interfaces[specInterface]?.["lan-config"] || {},
    )[0]?.[1]?.internal ||
    Object.entries(
      manifest.interfaces[specInterface]?.["tor-config"]?.["port-mapping"] ||
        {},
    )?.[0]?.[1]

  if (!internalPort) return null
  const serviceInterfaceId = `${specInterface}-${internalPort}`
  return serviceInterfaceId
}
async function convertToNewConfig(value: OldGetConfigRes) {
  const valueSpec: OldConfigSpec = matchOldConfigSpec.unsafeCast(value.spec)
  const spec = transformConfigSpec(valueSpec)
  if (!value.config) return { spec, config: null }
  const config = transformOldConfigToNew(valueSpec, value.config)
  return { spec, config }
}
