import * as T from "@start9labs/start-sdk/lib/types"
import * as fs from "fs/promises"

import { System } from "../../../Interfaces/System"
import { createUtils } from "@start9labs/start-sdk/lib/util"
import { matchManifest, Manifest } from "./matchManifest"
import { create } from "domain"
import { DockerProcedure } from "../../../Models/DockerProcedure"
import {DockerProcedureContainer} from '../../DockerProcedureContainer'

const MANIFEST_LOCATION = "/lib/startos/embassyManifest.json"
const EMBASSY_JS_LOCATION = "/usr/lib/javascript/embassy.js"
export class SystemForEmbassy implements System {
  moduleCode: Promise<unknown> = Promise.resolve()
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
    this.moduleCode = Promise.resolve().then(() => require(EMBASSY_JS_LOCATION))
  }
  async exit(effects: T.Effects): Promise<void> {
    await this.stop(effects)
  }
  async start(effects: T.Effects): Promise<void> {
    if (!!this.currentRunning) return
    const utils = createUtils(effects)
    const currentCommand: [string, ...string[]] = [
      this.manifest.main.entrypoint,
      ...this.manifest.main.args,
    ]

    await effects.setMainStatus({ status: "running" })
    this.currentRunning = await utils.runDaemon(currentCommand, {})
    this.currentRunning.wait().then(() => {
      effects.setMainStatus({ status: "stopped" })
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
        
        | `/health/${string}`
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
          case options.procedure.startsWith("/health/"):
            return this.health(effects, procedure[2], input)
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
      await using container = await DockerProcedureContainer.of(backup)
      await container.exec([backup.entrypoint, ...backup.args])
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async restoreBackup(effects: T.Effects): Promise<void> {
    const restoreBackup = this.manifest.backup.restore
    if (restoreBackup.type === "docker") {
      await using container = await DockerProcedureContainer.of(restoreBackup)
      await container.exec([restoreBackup.entrypoint, ...restoreBackup.args])
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async getConfig(effects: T.Effects): Promise<T.ConfigRes> {
    const config = this.manifest.config?.get
    if (!config) return {spec:{}}
    if (config.type === "docker") {
      await using container = await DockerProcedureContainer.of(config)
      return JSON.parse((await container.exec([config.entrypoint, ...config.args])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async setConfig(effects: T.Effects, newConfig: unknown): Promise<T.SetResult> {
    const setConfigValue = this.manifest.config?.set
    if (!setConfigValue) return {signal:"SIGTERM", "depends-on":{}}
    // TODO Deal with the pointers
    if (setConfigValue.type === "docker") {
      await using container = await DockerProcedureContainer.of(setConfigValue)
      return JSON.parse((await container.exec([setConfigValue.entrypoint, ...setConfigValue.args, JSON.stringify(newConfig)])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async migration(effects: T.Effects, fromVersion: unknown): Promise<T.MigrationRes> {
    // //todo filter
    // const setConfigValue = this.manifest.migrations
    // if (!setConfigValue) return {configured:true}
    // // TODO Deal with the pointers
    // if (setConfigValue.type === "docker") {
    //   await using container = await DockerProcedureContainer.of(setConfigValue)
    //   return JSON.parse((await container.exec([setConfigValue.entrypoint, ...setConfigValue.args, JSON.stringify(fromVersion)])).stdout)
    // } else {
    //   throw new Error("Method not implemented.")
    // }
    throw new Error("Not implemented")
  }
  async properties(effects: T.Effects): Promise<unknown> {
    const setConfigValue = this.manifest.properties
    if (!setConfigValue) return {}
    if (setConfigValue.type === "docker") {
      await using container = await DockerProcedureContainer.of(setConfigValue)
      return JSON.parse((await container.exec([setConfigValue.entrypoint, ...setConfigValue.args])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async health(
    effects: T.Effects,
    healthId: string,
    timeSinceStarted: unknown,
  ): Promise<void> {
    const healthProcedure = this.manifest["health-checks"][healthId]?.implementation
    if (!healthProcedure) return
    if (healthProcedure.type === "docker") {
      await using container = await DockerProcedureContainer.of(healthProcedure)
      return JSON.parse((await container.exec([healthProcedure.entrypoint, ...healthProcedure.args, JSON.stringify(timeSinceStarted)])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async action(
    effects: T.Effects,
    actionId: string,
    formData: unknown,
  ): Promise<T.ActionResult> {
    const actionProcedure = this.manifest.actions[actionId]?.implementation
    if (!actionProcedure) return {message: "Action not found", value: null}
    if (actionProcedure.type === "docker") {
      await using container = await DockerProcedureContainer.of(actionProcedure)
      return JSON.parse((await container.exec([actionProcedure.entrypoint, ...actionProcedure.args, JSON.stringify(formData)])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async dependenciesCheck(
    effects: T.Effects,
    id: string,
    oldConfig: unknown,
  ): Promise<object> {
    const actionProcedure = this.manifest.dependencies[id]?.config?.check
    if (!actionProcedure) return {message: "Action not found", value: null}
    if (actionProcedure.type === "docker") {
      await using container = await DockerProcedureContainer.of(actionProcedure)
      return JSON.parse((await container.exec([actionProcedure.entrypoint, ...actionProcedure.args, JSON.stringify(oldConfig)])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  async dependenciesAutoconfig(
    effects: T.Effects,
    id: string,
    oldConfig: unknown,
  ): Promise<void> {
    const actionProcedure = this.manifest.dependencies[id]?.config?.["auto-configure"]
    if (!actionProcedure) return 
    if (actionProcedure.type === "docker") {
      await using container = await DockerProcedureContainer.of(actionProcedure)
      return JSON.parse((await container.exec([actionProcedure.entrypoint, ...actionProcedure.args, JSON.stringify(oldConfig)])).stdout)
    } else {
      throw new Error("Method not implemented.")
    }
  }
  sandbox(
    effects: T.Effects,
    options: {
      procedure:
        | "/createBackup"
        | "/restoreBackup"
        | "/getConfig"
        | "/setConfig"
        | "migration"
        | "/properties"
        | `/health/${string}`
        | `/action/${string}`
        | `/dependencies/${string}/check`
        | `/dependencies/${string}/autoConfigure`
      input: unknown
      timeout?: number | undefined
    },
  ): Promise<void> {
    throw new Error("Method not implemented.")
  }
}
