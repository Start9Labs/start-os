import * as T from "@start9labs/start-sdk/lib/types"
import * as fs from "fs/promises"

import { PolyfillEffects } from "./polyfillEffects"
import { ExecuteResult, System } from "../../../Interfaces/System"
import { createUtils } from "@start9labs/start-sdk/lib/util"
import { matchManifest, Manifest, Procedure } from "./matchManifest"
import { create } from "domain"
import * as childProcess from "node:child_process"
import { Volume } from "../../../Models/Volume"
import { DockerProcedure } from "../../../Models/DockerProcedure"
import { DockerProcedureContainer } from "./DockerProcedureContainer"
import { promisify } from "node:util"
import * as U from "./oldEmbassyTypes"
import { MainLoop } from "./MainLoop"
import { EmVer } from "@start9labs/start-sdk/lib/emverLite/mod"
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
} from "ts-matches"
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { JsonPath, unNestPath } from "../../../Models/JsonPath"
import { HostSystem } from "../../../Interfaces/HostSystem"

type Optional<A> = A | undefined | null
function todo(): never {
  throw new Error("Not implemented")
}
const execFile = promisify(childProcess.execFile)

const MANIFEST_LOCATION = "/usr/lib/startos/package/embassyManifest.json"
const EMBASSY_JS_LOCATION = "/usr/lib/startos/package/embassy.js"
const EMBASSY_POINTER_PATH_PREFIX = "/embassyConfig"

export class SystemForEmbassy implements System {
  currentRunning: MainLoop | undefined
  static async of(manifestLocation: string = MANIFEST_LOCATION) {
    const moduleCode = await import(EMBASSY_JS_LOCATION).catch((_) =>
      require(EMBASSY_JS_LOCATION),
    )
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
  ): Promise<ExecuteResult> {
    return this._execute(effects, options)
      .then((x) =>
        matches(x)
          .when(
            object({
              result: any,
            }),
            (x) => ({
              ok: x.result,
            }),
          )
          .when(
            object({
              error: string,
            }),
            (x) => ({
              err: {
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
              err: {
                code,
                message,
              },
            }),
          )
          .defaultTo({ ok: x }),
      )
      .catch((error) => ({
        err: {
          code: 0,
          message: "" + error,
        },
      }))
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
        return this.createBackup(effects)
      case "/backup/restore":
        return this.restoreBackup(effects)
      case "/config/get":
        return this.getConfig(effects)
      case "/config/set":
        return this.setConfig(effects, input)
      case "/actions/metadata":
        return todo()
      case "/init":
        return this.init(effects, string.optional().unsafeCast(input))
      case "/uninit":
        return this.uninit(effects, string.optional().unsafeCast(input))
      case "/main/start":
        return this.mainStart(effects)
      case "/main/stop":
        return this.mainStop(effects)
      default:
        const procedures = unNestPath(options.procedure)
        switch (true) {
          case procedures[1] === "actions" && procedures[3] === "get":
            return this.action(effects, procedures[2], input)
          case procedures[1] === "actions" && procedures[3] === "run":
            return this.action(effects, procedures[2], input)
          case procedures[1] === "dependencies" && procedures[3] === "query":
            return this.dependenciesAutoconfig(effects, procedures[2], input)

          case procedures[1] === "dependencies" && procedures[3] === "update":
            return this.dependenciesAutoconfig(effects, procedures[2], input)
        }
    }
  }
  private async init(
    effects: HostSystemStartOs,
    previousVersion: Optional<string>,
  ): Promise<void> {
    this.mountMainVolumes()
    if (previousVersion) await this.migration(effects, previousVersion)
    await this.properties(effects)
    await effects.setMainStatus({ status: "stopped" })
  }
  private async uninit(
    effects: HostSystemStartOs,
    nextVersion: Optional<string>,
  ): Promise<void> {
    // TODO Do a migration down if the version exists
    await effects.setMainStatus({ status: "stopped" })
  }
  private async mainStart(effects: HostSystemStartOs): Promise<void> {
    if (!!this.currentRunning) return

    this.currentRunning = new MainLoop(this, effects, () =>
      this.properties(effects),
    )
  }
  private async mainStop(
    effects: HostSystemStartOs,
    options?: { timeout?: number },
  ): Promise<void> {
    const { currentRunning } = this
    delete this.currentRunning
    if (currentRunning) {
      await currentRunning.clean({
        timeout: options?.timeout || this.manifest.main["sigterm-timeout"],
      })
    }
  }
  private async createBackup(effects: HostSystemStartOs): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.of(backup)
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(new PolyfillEffects(effects))
    }
  }
  private async restoreBackup(effects: HostSystemStartOs): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.of(restoreBackup)
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(new PolyfillEffects(effects))
    }
  }
  private async getConfig(effects: HostSystemStartOs): Promise<T.ConfigRes> {
    return this.getConfigUncleaned(effects).then(removePointers)
  }
  private async getConfigUncleaned(
    effects: HostSystemStartOs,
  ): Promise<T.ConfigRes> {
    const config = this.manifest.config?.get
    if (!config) return { spec: {} }
    if (config.type === "docker") {
      const container = await DockerProcedureContainer.of(config)
      return JSON.parse(
        (await container.exec([config.entrypoint, ...config.args])).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.getConfig
      if (!method) throw new Error("Expecting that the method getConfig exists")
      return (await method(new PolyfillEffects(effects)).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })) as any
    }
  }
  private async setConfig(
    effects: HostSystemStartOs,
    newConfigWithoutPointers: unknown,
  ): Promise<T.SetResult> {
    const newConfig = structuredClone(newConfigWithoutPointers)
    await updateConfig(
      effects,
      await this.getConfigUncleaned(effects).then((x) => x.spec),
      newConfig,
    )
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return { signal: "SIGTERM", "depends-on": {} }
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.of(setConfigValue)
      return JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
            JSON.stringify(newConfig),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.setConfig
      if (!method) throw new Error("Expecting that the method setConfig exists")
      return await method(
        new PolyfillEffects(effects),
        newConfig as U.Config,
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    }
  }
  private async migration(
    effects: HostSystemStartOs,
    fromVersion: string,
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
        const container = await DockerProcedureContainer.of(procedure)
        return JSON.parse(
          (
            await container.exec([
              procedure.entrypoint,
              ...procedure.args,
              JSON.stringify(fromVersion),
            ])
          ).stdout,
        )
      } else {
        const moduleCode = await this.moduleCode
        const method = moduleCode.migration
        if (!method)
          throw new Error("Expecting that the method migration exists")
        return (await method(
          new PolyfillEffects(effects),
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
  private async properties(effects: HostSystemStartOs): Promise<undefined> {
    // TODO BLU-J set the properties ever so often
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) return
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.of(setConfigValue)
      return JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.properties
      if (!method)
        throw new Error("Expecting that the method properties exists")
      await method(new PolyfillEffects(effects)).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    }
  }
  private async health(
    effects: HostSystemStartOs,
    healthId: string,
    timeSinceStarted: unknown,
  ): Promise<void> {
    const healthProcedure = this.manifest["health-checks"][healthId]
    if (!healthProcedure) return
    if (healthProcedure.type === "docker") {
      const container = await DockerProcedureContainer.of(healthProcedure)
      return JSON.parse(
        (
          await container.exec([
            healthProcedure.entrypoint,
            ...healthProcedure.args,
            JSON.stringify(timeSinceStarted),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.health?.[healthId]
      if (!method) throw new Error("Expecting that the method health exists")
      await method(new PolyfillEffects(effects), Number(timeSinceStarted)).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )
    }
  }
  private async action(
    effects: HostSystemStartOs,
    actionId: string,
    formData: unknown,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions?.[actionId]?.implementation
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.of(actionProcedure)
      return JSON.parse(
        (
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(formData),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.action?.[actionId]
      if (!method) throw new Error("Expecting that the method action exists")
      return (await method(new PolyfillEffects(effects), formData as any).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  private async dependenciesCheck(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies?.[id]?.config?.check
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.of(actionProcedure)
      return JSON.parse(
        (
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(oldConfig),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.dependencies?.[id]?.check
      if (!method)
        throw new Error(
          `Expecting that the method dependency check ${id} exists`,
        )
      return (await method(new PolyfillEffects(effects), oldConfig as any).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  private async dependenciesAutoconfig(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
  ): Promise<void> {
    const moduleCode = await this.moduleCode
    const method = moduleCode.dependencies?.[id]?.autoConfigure
    if (!method)
      throw new Error(
        `Expecting that the method dependency autoConfigure ${id} exists`,
      )
    return (await method(new PolyfillEffects(effects), oldConfig as any).then(
      (x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      },
    )) as any
  }
  private async sandbox(
    effects: HostSystemStartOs,
    options: {
      procedure:
        | "/createBackup"
        | "/restoreBackup"
        | "/getConfig"
        | "/setConfig"
        | "migration"
        | "/properties"
        | `/action/${string}`
        | `/dependencies/${string}/check`
        | `/dependencies/${string}/autoConfigure`
      input: unknown
      timeout?: number | undefined
    },
  ): Promise<unknown> {
    const input = options.input
    switch (options.procedure) {
      case "/createBackup":
        return this.roCreateBackup(effects)
      case "/restoreBackup":
        return this.roRestoreBackup(effects)
      case "/getConfig":
        return this.roGetConfig(effects)
      case "/setConfig":
        return this.roSetConfig(effects, input)
      case "migration":
        return this.roMigration(effects, input)
      case "/properties":
        return this.roProperties(effects)
      default:
        const procedure = options.procedure.split("/")
        switch (true) {
          case options.procedure.startsWith("/action/"):
            return this.roAction(effects, procedure[2], input)
          case options.procedure.startsWith("/dependencies/") &&
            procedure[3] === "check":
            return this.roDependenciesCheck(effects, procedure[2], input)

          case options.procedure.startsWith("/dependencies/") &&
            procedure[3] === "autoConfigure":
            return this.roDependenciesAutoconfig(effects, procedure[2], input)
        }
    }
  }

  private async roCreateBackup(effects: HostSystemStartOs): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(backup)
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(new PolyfillEffects(effects))
    }
  }
  private async roRestoreBackup(effects: HostSystemStartOs): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(restoreBackup)
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(new PolyfillEffects(effects))
    }
  }
  private async roGetConfig(effects: HostSystemStartOs): Promise<T.ConfigRes> {
    const config = this.manifest.config?.get
    if (!config) return { spec: {} }
    if (config.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(config)
      return JSON.parse(
        (await container.exec([config.entrypoint, ...config.args])).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.getConfig
      if (!method) throw new Error("Expecting that the method getConfig exists")
      return (await method(new PolyfillEffects(effects)).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })) as any
    }
  }
  private async roSetConfig(
    effects: HostSystemStartOs,
    newConfig: unknown,
  ): Promise<T.SetResult> {
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return { signal: "SIGTERM", "depends-on": {} }
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(
        setConfigValue,
      )
      return JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
            JSON.stringify(newConfig),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.setConfig
      if (!method) throw new Error("Expecting that the method setConfig exists")
      return await method(
        new PolyfillEffects(effects),
        newConfig as U.Config,
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    }
  }
  private async roMigration(
    effects: HostSystemStartOs,
    fromVersion: unknown,
  ): Promise<T.MigrationRes> {
    throw new Error("Migrations should never be ran in the sandbox mode")
  }
  private async roProperties(effects: HostSystemStartOs): Promise<unknown> {
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) return {}
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(
        setConfigValue,
      )
      return JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.properties
      if (!method)
        throw new Error("Expecting that the method properties exists")
      return await method(new PolyfillEffects(effects)).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    }
  }
  private async roHealth(
    effects: HostSystemStartOs,
    healthId: string,
    timeSinceStarted: unknown,
  ): Promise<void> {
    const healthProcedure = this.manifest["health-checks"][healthId]
    if (!healthProcedure) return
    if (healthProcedure.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(
        healthProcedure,
      )
      return JSON.parse(
        (
          await container.exec([
            healthProcedure.entrypoint,
            ...healthProcedure.args,
            JSON.stringify(timeSinceStarted),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.health?.[healthId]
      if (!method) throw new Error("Expecting that the method health exists")
      await method(new PolyfillEffects(effects), Number(timeSinceStarted)).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )
    }
  }
  private async roAction(
    effects: HostSystemStartOs,
    actionId: string,
    formData: unknown,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions?.[actionId]?.implementation
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(
        actionProcedure,
      )
      return JSON.parse(
        (
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(formData),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.action?.[actionId]
      if (!method) throw new Error("Expecting that the method action exists")
      return (await method(new PolyfillEffects(effects), formData as any).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  private async roDependenciesCheck(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies?.[id]?.config?.check
    if (!actionProcedure) return { message: "Action not found", value: null }
    if (actionProcedure.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(
        actionProcedure,
      )
      return JSON.parse(
        (
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(oldConfig),
          ])
        ).stdout,
      )
    } else {
      const moduleCode = await this.moduleCode
      const method = moduleCode.dependencies?.[id]?.check
      if (!method)
        throw new Error(
          `Expecting that the method dependency check ${id} exists`,
        )
      return (await method(new PolyfillEffects(effects), oldConfig as any).then(
        (x) => {
          if ("result" in x) return x.result
          if ("error" in x) throw new Error("Error getting config: " + x.error)
          throw new Error("Error getting config: " + x["error-code"][1])
        },
      )) as any
    }
  }
  private async roDependenciesAutoconfig(
    effects: HostSystemStartOs,
    id: string,
    oldConfig: unknown,
  ): Promise<void> {
    const moduleCode = await this.moduleCode
    const method = moduleCode.dependencies?.[id]?.autoConfigure
    if (!method)
      throw new Error(
        `Expecting that the method dependency autoConfigure ${id} exists`,
      )
    return (await method(new PolyfillEffects(effects), oldConfig as any).then(
      (x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      },
    )) as any
  }
  private async mountMainVolumes() {
    const { main } = this.manifest
    const { mounts } = main
    for (const imageId in mounts) {
      try {
        const pathToMount = mounts[imageId]
        if (await fs.stat(pathToMount).catch(() => false)) continue
        const volume = new Volume(imageId)
        await execFile("mount", [
          "--target",
          pathToMount,
          "--source",
          volume.path,
        ])
      } catch (error) {
        console.error(error)
      }
    }
  }
}
async function removePointers(value: T.ConfigRes): Promise<T.ConfigRes> {
  const startingSpec = structuredClone(value.spec)
  const spec = cleanSpecOfPointers(startingSpec)

  return { ...value, spec }
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
      mutConfigValue[key] = await effects.embassyGetInterface({
        target: specValue.target,
        packageId: specValue["package-id"],
        interface: specValue["interface"],
      })
    }
  }
}
