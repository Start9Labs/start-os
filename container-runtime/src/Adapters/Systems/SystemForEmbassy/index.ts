import { types as T, util, EmVer, Utils } from "@start9labs/start-sdk"
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
} from "ts-matches"
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { JsonPath, unNestPath } from "../../../Models/JsonPath"
import { RpcResult, matchRpcResult } from "../../RpcListener"

type Optional<A> = A | undefined | null
function todo(): never {
  throw new Error("Not implemented")
}
const execFile = promisify(childProcess.execFile)

const MANIFEST_LOCATION = "/usr/lib/startos/package/embassyManifest.json"
const EMBASSY_JS_LOCATION = "/usr/lib/startos/package/embassy.js"
const EMBASSY_POINTER_PATH_PREFIX = "/embassyConfig"

const matchPackagePropertyObject = object({
  value: any,
  type: literal("object"),
  description: string,
})

const matchPackagePropertyString = object(
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

const matchProperties = object({
  version: literal(2),
  data: any,
})

type ExportUi = {
  value: string
  title: string
  description?: string | undefined
  masked?: boolean | undefined
  copyable?: boolean | undefined
  qr?: boolean | undefined
}

function propertiesToExportUi(properties: unknown): ExportUi[] {
  if (!object.test(properties)) return []
  const paths: ExportUi[] = []
  for (const key in properties) {
    const value: unknown = (properties as any)[key]
    if (matchPackagePropertyObject.test(value)) {
      paths.push(...propertiesToExportUi(value))
      continue
    }
    if (!matchPackagePropertyString.test(value)) continue
    paths.push({
      value: value.value,
      title: key,
      description: value.description,
      masked: value.masked,
      copyable: value.copyable,
      qr: value.qr,
    })
  }
  return paths
}

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
    throw new Error(`Could not find the path for ${options.procedure}`)
  }
  private async init(
    effects: HostSystemStartOs,
    previousVersion: Optional<string>,
  ): Promise<void> {
    if (previousVersion) await this.migration(effects, previousVersion)
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
  ): Promise<Duration> {
    const { currentRunning } = this
    delete this.currentRunning
    if (currentRunning) {
      await currentRunning.clean({
        timeout: options?.timeout || this.manifest.main["sigterm-timeout"],
      })
    }
    return duration(this.manifest.main["sigterm-timeout"], "s")
  }
  private async createBackup(effects: HostSystemStartOs): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        backup,
        this.manifest.volumes,
      )
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(
        new PolyfillEffects(effects, this.manifest),
      )
    }
  }
  private async restoreBackup(effects: HostSystemStartOs): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        restoreBackup,
        this.manifest.volumes,
      )
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(
        new PolyfillEffects(effects, this.manifest),
      )
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
      const container = await DockerProcedureContainer.of(
        effects,
        config,
        this.manifest.volumes,
      )
      // TODO: yaml
      return JSON.parse(
        (
          await container.exec([config.entrypoint, ...config.args])
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
      const container = await DockerProcedureContainer.of(
        effects,
        setConfigValue,
        this.manifest.volumes,
      )
      return JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
            JSON.stringify(newConfig),
          ])
        ).stdout.toString(),
      )
    } else if (setConfigValue.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.setConfig
      if (!method) throw new Error("Expecting that the method setConfig exists")
      return await method(
        new PolyfillEffects(effects, this.manifest),
        newConfig as U.Config,
      ).then((x): T.SetResult => {
        if ("result" in x)
          return {
            "depends-on": x.result["depends-on"],
            signal: x.result.signal === "SIGEMT" ? "SIGTERM" : x.result.signal,
          }
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    } else {
      return {
        "depends-on": {},
        signal: "SIGTERM",
      }
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
        const container = await DockerProcedureContainer.of(
          effects,
          procedure,
          this.manifest.volumes,
        )
        return JSON.parse(
          (
            await container.exec([
              procedure.entrypoint,
              ...procedure.args,
              JSON.stringify(fromVersion),
            ])
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
  private async properties(effects: HostSystemStartOs): Promise<undefined> {
    // TODO BLU-J set the properties ever so often
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) return
    if (setConfigValue.type === "docker") {
      const container = await DockerProcedureContainer.of(
        effects,
        setConfigValue,
        this.manifest.volumes,
      )
      const properties = JSON.parse(
        (
          await container.exec([
            setConfigValue.entrypoint,
            ...setConfigValue.args,
          ])
        ).stdout.toString(),
      )
      if (!matchProperties.test(properties)) return
      const exposeUis = propertiesToExportUi(properties.data)
      await effects.store.set<any, any>({
        path: "/properties",
        value: exposeUis.map((x) => x.value),
      })
      await effects.exposeUi(
        exposeUis.map((x, i) => ({ ...x, path: `/properties/${i}` }) as any),
      )
    } else if (setConfigValue.type === "script") {
      const moduleCode = this.moduleCode
      const method = moduleCode.properties
      if (!method)
        throw new Error("Expecting that the method properties exists")
      const properties = await method(
        new PolyfillEffects(effects, this.manifest),
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
      if (!matchProperties.test(properties)) return
      const exposeUis = propertiesToExportUi(properties.data)
      await effects.store.set<any, any>({
        path: "/properties",
        value: exposeUis.map((x) => x.value),
      })
      await effects.exposeUi(
        exposeUis.map((x, i) => ({ ...x, path: `/properties/${i}` }) as any),
      )
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
      const container = await DockerProcedureContainer.of(
        effects,
        healthProcedure,
        this.manifest.volumes,
      )
      return JSON.parse(
        (
          await container.exec([
            healthProcedure.entrypoint,
            ...healthProcedure.args,
            JSON.stringify(timeSinceStarted),
          ])
        ).stdout.toString(),
      )
    } else if (healthProcedure.type === "script") {
      const moduleCode = await this.moduleCode
      const method = moduleCode.health?.[healthId]
      if (!method) throw new Error("Expecting that the method health exists")
      await method(
        new PolyfillEffects(effects, this.manifest),
        Number(timeSinceStarted),
      ).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
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
      const container = await DockerProcedureContainer.of(
        effects,
        actionProcedure,
        this.manifest.volumes,
      )
      return JSON.parse(
        (
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(formData),
          ])
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
          await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(oldConfig),
          ])
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
  ): Promise<void> {
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
  // private async sandbox(
  //   effects: HostSystemStartOs,
  //   options: {
  //     procedure:
  //       | "/createBackup"
  //       | "/restoreBackup"
  //       | "/getConfig"
  //       | "/setConfig"
  //       | "migration"
  //       | "/properties"
  //       | `/action/${string}`
  //       | `/dependencies/${string}/check`
  //       | `/dependencies/${string}/autoConfigure`
  //     input: unknown
  //     timeout?: number | undefined
  //   },
  // ): Promise<unknown> {
  //   const input = options.input
  //   switch (options.procedure) {
  //     case "/createBackup":
  //       return this.roCreateBackup(effects)
  //     case "/restoreBackup":
  //       return this.roRestoreBackup(effects)
  //     case "/getConfig":
  //       return this.roGetConfig(effects)
  //     case "/setConfig":
  //       return this.roSetConfig(effects, input)
  //     case "migration":
  //       return this.roMigration(effects, input)
  //     case "/properties":
  //       return this.roProperties(effects)
  //     default:
  //       const procedure = options.procedure.split("/")
  //       switch (true) {
  //         case options.procedure.startsWith("/action/"):
  //           return this.roAction(effects, procedure[2], input)
  //         case options.procedure.startsWith("/dependencies/") &&
  //           procedure[3] === "check":
  //           return this.roDependenciesCheck(effects, procedure[2], input)

  //         case options.procedure.startsWith("/dependencies/") &&
  //           procedure[3] === "autoConfigure":
  //           return this.roDependenciesAutoconfig(effects, procedure[2], input)
  //       }
  //   }
  // }

  // private async roCreateBackup(effects: HostSystemStartOs): Promise<void> {
  //   const backup = this.manifest.backup.create
  //   if (backup.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(backup)
  //     await container.exec([backup.entrypoint, ...backup.args])
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     await moduleCode.createBackup?.(new PolyfillEffects(effects))
  //   }
  // }
  // private async roRestoreBackup(effects: HostSystemStartOs): Promise<void> {
  //   const restoreBackup = this.manifest.backup.restore
  //   if (restoreBackup.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(restoreBackup)
  //     await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     await moduleCode.restoreBackup?.(new PolyfillEffects(effects))
  //   }
  // }
  // private async roGetConfig(effects: HostSystemStartOs): Promise<T.ConfigRes> {
  //   const config = this.manifest.config?.get
  //   if (!config) return { spec: {} }
  //   if (config.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(config)
  //     return JSON.parse(
  //       (await container.exec([config.entrypoint, ...config.args])).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.getConfig
  //     if (!method) throw new Error("Expecting that the method getConfig exists")
  //     return (await method(new PolyfillEffects(effects)).then((x) => {
  //       if ("result" in x) return x.result
  //       if ("error" in x) throw new Error("Error getting config: " + x.error)
  //       throw new Error("Error getting config: " + x["error-code"][1])
  //     })) as any
  //   }
  // }
  // private async roSetConfig(
  //   effects: HostSystemStartOs,
  //   newConfig: unknown,
  // ): Promise<T.SetResult> {
  //   const setConfigValue = this.manifest.config?.set
  //   if (!setConfigValue) return { signal: "SIGTERM", "depends-on": {} }
  //   if (setConfigValue.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(
  //       setConfigValue,
  //     )
  //     return JSON.parse(
  //       (
  //         await container.exec([
  //           setConfigValue.entrypoint,
  //           ...setConfigValue.args,
  //           JSON.stringify(newConfig),
  //         ])
  //       ).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.setConfig
  //     if (!method) throw new Error("Expecting that the method setConfig exists")
  //     return await method(
  //       new PolyfillEffects(effects),
  //       newConfig as U.Config,
  //     ).then((x) => {
  //       if ("result" in x) return x.result
  //       if ("error" in x) throw new Error("Error getting config: " + x.error)
  //       throw new Error("Error getting config: " + x["error-code"][1])
  //     })
  //   }
  // }
  // private async roMigration(
  //   effects: HostSystemStartOs,
  //   fromVersion: unknown,
  // ): Promise<T.MigrationRes> {
  //   throw new Error("Migrations should never be ran in the sandbox mode")
  // }
  // private async roProperties(effects: HostSystemStartOs): Promise<unknown> {
  //   const setConfigValue = this.manifest.properties
  //   if (!setConfigValue) return {}
  //   if (setConfigValue.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(
  //       setConfigValue,
  //     )
  //     return JSON.parse(
  //       (
  //         await container.exec([
  //           setConfigValue.entrypoint,
  //           ...setConfigValue.args,
  //         ])
  //       ).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.properties
  //     if (!method)
  //       throw new Error("Expecting that the method properties exists")
  //     return await method(new PolyfillEffects(effects)).then((x) => {
  //       if ("result" in x) return x.result
  //       if ("error" in x) throw new Error("Error getting config: " + x.error)
  //       throw new Error("Error getting config: " + x["error-code"][1])
  //     })
  //   }
  // }
  // private async roHealth(
  //   effects: HostSystemStartOs,
  //   healthId: string,
  //   timeSinceStarted: unknown,
  // ): Promise<void> {
  //   const healthProcedure = this.manifest["health-checks"][healthId]
  //   if (!healthProcedure) return
  //   if (healthProcedure.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(
  //       healthProcedure,
  //     )
  //     return JSON.parse(
  //       (
  //         await container.exec([
  //           healthProcedure.entrypoint,
  //           ...healthProcedure.args,
  //           JSON.stringify(timeSinceStarted),
  //         ])
  //       ).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.health?.[healthId]
  //     if (!method) throw new Error("Expecting that the method health exists")
  //     await method(new PolyfillEffects(effects), Number(timeSinceStarted)).then(
  //       (x) => {
  //         if ("result" in x) return x.result
  //         if ("error" in x) throw new Error("Error getting config: " + x.error)
  //         throw new Error("Error getting config: " + x["error-code"][1])
  //       },
  //     )
  //   }
  // }
  // private async roAction(
  //   effects: HostSystemStartOs,
  //   actionId: string,
  //   formData: unknown,
  // ): Promise<T.ActionResult> {
  //   const actionProcedure = this.manifest.actions?.[actionId]?.implementation
  //   if (!actionProcedure) return { message: "Action not found", value: null }
  //   if (actionProcedure.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(
  //       actionProcedure,
  //     )
  //     return JSON.parse(
  //       (
  //         await container.exec([
  //           actionProcedure.entrypoint,
  //           ...actionProcedure.args,
  //           JSON.stringify(formData),
  //         ])
  //       ).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.action?.[actionId]
  //     if (!method) throw new Error("Expecting that the method action exists")
  //     return (await method(new PolyfillEffects(effects), formData as any).then(
  //       (x) => {
  //         if ("result" in x) return x.result
  //         if ("error" in x) throw new Error("Error getting config: " + x.error)
  //         throw new Error("Error getting config: " + x["error-code"][1])
  //       },
  //     )) as any
  //   }
  // }
  // private async roDependenciesCheck(
  //   effects: HostSystemStartOs,
  //   id: string,
  //   oldConfig: unknown,
  // ): Promise<object> {
  //   const actionProcedure = this.manifest.dependencies?.[id]?.config?.check
  //   if (!actionProcedure) return { message: "Action not found", value: null }
  //   if (actionProcedure.type === "docker") {
  //     const container = await DockerProcedureContainer.readonlyOf(
  //       actionProcedure,
  //     )
  //     return JSON.parse(
  //       (
  //         await container.exec([
  //           actionProcedure.entrypoint,
  //           ...actionProcedure.args,
  //           JSON.stringify(oldConfig),
  //         ])
  //       ).stdout,
  //     )
  //   } else {
  //     const moduleCode = await this.moduleCode
  //     const method = moduleCode.dependencies?.[id]?.check
  //     if (!method)
  //       throw new Error(
  //         `Expecting that the method dependency check ${id} exists`,
  //       )
  //     return (await method(new PolyfillEffects(effects), oldConfig as any).then(
  //       (x) => {
  //         if ("result" in x) return x.result
  //         if ("error" in x) throw new Error("Error getting config: " + x.error)
  //         throw new Error("Error getting config: " + x["error-code"][1])
  //       },
  //     )) as any
  //   }
  // }
  // private async roDependenciesAutoconfig(
  //   effects: HostSystemStartOs,
  //   id: string,
  //   oldConfig: unknown,
  // ): Promise<void> {
  //   const moduleCode = await this.moduleCode
  //   const method = moduleCode.dependencies?.[id]?.autoConfigure
  //   if (!method)
  //     throw new Error(
  //       `Expecting that the method dependency autoConfigure ${id} exists`,
  //     )
  //   return (await method(new PolyfillEffects(effects), oldConfig as any).then(
  //     (x) => {
  //       if ("result" in x) return x.result
  //       if ("error" in x) throw new Error("Error getting config: " + x.error)
  //       throw new Error("Error getting config: " + x["error-code"][1])
  //     },
  //   )) as any
  // }
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
  const utils = util.createUtils(effects)
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
      const filled = await utils.serviceInterface
        .get({
          packageId: specValue["package-id"],
          id: specValue.interface,
        })
        .once()
        .catch(() => null)

      mutConfigValue[key] =
        filled === null
          ? ""
          : specValue.target === "lan-address"
            ? filled.addressInfo.localHostnames[0]
            : filled.addressInfo.onionHostnames[0]
    }
  }
}
