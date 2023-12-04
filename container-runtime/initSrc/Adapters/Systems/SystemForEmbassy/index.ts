import * as T from "@start9labs/start-sdk/lib/types"
import * as fs from "fs/promises"

import { PolyfillEffects } from "./polyfillEffects"
import { System } from "../../../Interfaces/System"
import { createUtils } from "@start9labs/start-sdk/lib/util"
import { matchManifest, Manifest, Procedure } from "./matchManifest"
import { create } from "domain"
import { DockerProcedure } from "../../../Models/DockerProcedure"
import { DockerProcedureContainer } from "./DockerProcedureContainer"
import * as U from "./oldEmbassyTypes"
import { EmbassyHealth } from "./EmbassyHealth"
import { EmVer } from "@start9labs/start-sdk/lib/emverLite/mod"
import { string } from "ts-matches"

const MANIFEST_LOCATION = "/lib/startos/embassyManifest.json"
const EMBASSY_JS_LOCATION = "/usr/lib/javascript/embassy.js"
export class SystemForEmbassy implements System {
  moduleCode: Promise<Partial<U.ExpectedExports>> = Promise.resolve({})
  currentRunning: T.DaemonReturned | undefined
  static async of(manifestLocation: string = MANIFEST_LOCATION) {
    return fs.readFile(manifestLocation, "utf-8").then((manifest) => {
      return new SystemForEmbassy(
        matchManifest.unsafeCast(JSON.parse(manifest)),
      )
    })
  }
  constructor(readonly manifest: Manifest) {}
  async init(effects: T.Effects): Promise<void> {
    this.moduleCode = Promise.resolve()
      .then(() => require(EMBASSY_JS_LOCATION))
      .catch(() => ({}))
    await effects.setMainStatus({ status: "stopped" })
  }
  async exit(effects: T.Effects): Promise<void> {
    await this.stop(effects)
  }
  async start(effects: T.Effects): Promise<void> {
    if (!!this.currentRunning) return
    const utils = createUtils(effects)

    new EmbassyHealth(this, effects).during(async () => {
      const currentCommand: [string, ...string[]] = [
        this.manifest.main.entrypoint,
        ...this.manifest.main.args,
      ]

      await effects.setMainStatus({ status: "running" })
      this.currentRunning = await utils.runDaemon(currentCommand, {})
      this.currentRunning.wait().then(() => {
        effects.setMainStatus({ status: "stopped" })
      })
    })
  }
  async stop(
    effects: T.Effects,
    options?: { timeout?: number },
  ): Promise<void> {
    if (this.currentRunning) {
      await this.currentRunning.term({
        signal: "SIGTERM",
        timeout: options?.timeout || this.manifest.main["sigterm-timeout"],
      })
    }
    this.currentRunning = undefined
  }
  async execute(
    effects: T.Effects,
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
        return this.createBackup(effects)
      case "/restoreBackup":
        return this.restoreBackup(effects)
      case "/getConfig":
        return this.getConfig(effects)
      case "/setConfig":
        return this.setConfig(effects, input)
      case "migration":
        return this.migration(effects, input)
      case "/properties":
        return this.properties(effects)
      default:
        const procedure = options.procedure.split("/")
        switch (true) {
          case options.procedure.startsWith("/action/"):
            return this.action(effects, procedure[2], input)
          case options.procedure.startsWith("/dependencies/") &&
            procedure[3] === "check":
            return this.dependenciesCheck(effects, procedure[2], input)

          case options.procedure.startsWith("/dependencies/") &&
            procedure[3] === "autoConfigure":
            return this.dependenciesAutoconfig(effects, procedure[2], input)
        }
    }
  }
  async createBackup(effects: T.Effects): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.of(backup)
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(new PolyfillEffects(effects))
    }
  }
  async restoreBackup(effects: T.Effects): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.of(restoreBackup)
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(new PolyfillEffects(effects))
    }
  }
  async getConfig(effects: T.Effects): Promise<T.ConfigRes> {
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
  async setConfig(
    effects: T.Effects,
    newConfig: unknown,
  ): Promise<T.SetResult> {
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return { signal: "SIGTERM", "depends-on": {} }
    // TODO Deal with the pointers
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
  async migration(
    effects: T.Effects,
    fromVersion: unknown,
  ): Promise<T.MigrationRes> {
    if (!string.test(fromVersion)) throw new Error("Expecting a string")
    const fromEmver = EmVer.from(fromVersion)
    const currentEmver = EmVer.from(this.manifest.version)

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
  async properties(effects: T.Effects): Promise<unknown> {
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) return {}
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
      return await method(new PolyfillEffects(effects)).then((x) => {
        if ("result" in x) return x.result
        if ("error" in x) throw new Error("Error getting config: " + x.error)
        throw new Error("Error getting config: " + x["error-code"][1])
      })
    }
  }
  async health(
    effects: T.Effects,
    healthId: string,
    timeSinceStarted: unknown,
  ): Promise<void> {
    const healthProcedure =
      this.manifest["health-checks"][healthId]?.implementation
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
  async action(
    effects: T.Effects,
    actionId: string,
    formData: unknown,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions[actionId]?.implementation
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
  async dependenciesCheck(
    effects: T.Effects,
    id: string,
    oldConfig: unknown,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies[id]?.config?.check
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
  async dependenciesAutoconfig(
    effects: T.Effects,
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
  async sandbox(
    effects: T.Effects,
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

  async roCreateBackup(effects: T.Effects): Promise<void> {
    const backup = this.manifest.backup.create
    if (backup.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(backup)
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.createBackup?.(new PolyfillEffects(effects))
    }
  }
  async roRestoreBackup(effects: T.Effects): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      const container = await DockerProcedureContainer.readonlyOf(restoreBackup)
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      const moduleCode = await this.moduleCode
      await moduleCode.restoreBackup?.(new PolyfillEffects(effects))
    }
  }
  async roGetConfig(effects: T.Effects): Promise<T.ConfigRes> {
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
  async roSetConfig(
    effects: T.Effects,
    newConfig: unknown,
  ): Promise<T.SetResult> {
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return { signal: "SIGTERM", "depends-on": {} }
    // TODO Deal with the pointers
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
  async roMigration(
    effects: T.Effects,
    fromVersion: unknown,
  ): Promise<T.MigrationRes> {
    throw new Error("Migrations should never be ran in the sandbox mode")
  }
  async roProperties(effects: T.Effects): Promise<unknown> {
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
  async roHealth(
    effects: T.Effects,
    healthId: string,
    timeSinceStarted: unknown,
  ): Promise<void> {
    const healthProcedure =
      this.manifest["health-checks"][healthId]?.implementation
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
  async roAction(
    effects: T.Effects,
    actionId: string,
    formData: unknown,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions[actionId]?.implementation
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
  async roDependenciesCheck(
    effects: T.Effects,
    id: string,
    oldConfig: unknown,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies[id]?.config?.check
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
  async roDependenciesAutoconfig(
    effects: T.Effects,
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
}
