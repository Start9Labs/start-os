import { ExecuteResult, Procedure, System } from "../../Interfaces/System"
import { unNestPath } from "../../Models/JsonPath"
import matches, { any, number, object, string, tuple } from "ts-matches"
import { Effects } from "../../Models/Effects"
import { RpcResult, matchRpcResult } from "../RpcListener"
import { duration } from "../../Models/Duration"
import { T, utils } from "@start9labs/start-sdk"
import { Volume } from "../../Models/Volume"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"
import { CallbackHolder } from "../../Models/CallbackHolder"
import { Optional } from "ts-matches/lib/parsers/interfaces"

export const STARTOS_JS_LOCATION = "/usr/lib/startos/package/index.js"

type RunningMain = {
  effects: MainEffects
  stop: () => Promise<void>
  callbacks: CallbackHolder
}

export class SystemForStartOs implements System {
  private runningMain: RunningMain | undefined

  static of() {
    return new SystemForStartOs(require(STARTOS_JS_LOCATION))
  }

  constructor(readonly abi: T.ABI) {}
  containerInit(): Promise<void> {
    throw new Error("Method not implemented.")
  }
  async packageInit(
    effects: Effects,
    previousVersion: Optional<string> = null,
    timeoutMs: number | null = null,
  ): Promise<void> {
    return void (await this.abi.init({ effects }))
  }
  async packageUninit(
    effects: Effects,
    nextVersion: Optional<string> = null,
    timeoutMs: number | null = null,
  ): Promise<void> {
    return void (await this.abi.uninit({ effects, nextVersion }))
  }
  async createBackup(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    return void (await this.abi.createBackup({
      effects,
      pathMaker: ((options) =>
        new Volume(options.volume, options.path).path) as T.PathMaker,
    }))
  }
  async restoreBackup(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    return void (await this.abi.restoreBackup({
      effects,
      pathMaker: ((options) =>
        new Volume(options.volume, options.path).path) as T.PathMaker,
    }))
  }
  getConfig(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<T.ConfigRes> {
    return this.abi.getConfig({ effects })
  }
  async setConfig(
    effects: Effects,
    input: { effects: Effects; input: Record<string, unknown> },
    timeoutMs: number | null,
  ): Promise<void> {
    const _: unknown = await this.abi.setConfig({ effects, input })
    return
  }
  migration(
    effects: Effects,
    fromVersion: string,
    timeoutMs: number | null,
  ): Promise<T.MigrationRes> {
    throw new Error("Method not implemented.")
  }
  properties(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<T.PropertiesReturn> {
    throw new Error("Method not implemented.")
  }
  async action(
    effects: Effects,
    id: string,
    formData: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult> {
    const action = (await this.abi.actions({ effects }))[id]
    if (!action) throw new Error(`Action ${id} not found`)
    return action.run({ effects })
  }
  dependenciesCheck(
    effects: Effects,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<any> {
    const dependencyConfig = this.abi.dependencyConfig[id]
    if (!dependencyConfig) throw new Error(`dependencyConfig ${id} not found`)
    return dependencyConfig.query({ effects })
  }
  async dependenciesAutoconfig(
    effects: Effects,
    id: string,
    remoteConfig: unknown,
    timeoutMs: number | null,
  ): Promise<void> {
    const dependencyConfig = this.abi.dependencyConfig[id]
    if (!dependencyConfig) throw new Error(`dependencyConfig ${id} not found`)
    const queryResults = await this.getConfig(effects, timeoutMs)
    return void (await dependencyConfig.update({
      queryResults,
      remoteConfig,
    })) // TODO
  }
  async actionsMetadata(effects: T.Effects): Promise<T.ActionMetadata[]> {
    return this.abi.actionsMetadata({ effects })
  }

  async init(): Promise<void> {}

  async exit(): Promise<void> {}

  async start(effects: MainEffects): Promise<void> {
    if (this.runningMain) await this.stop()
    let mainOnTerm: () => Promise<void> | undefined
    const started = async (onTerm: () => Promise<void>) => {
      await effects.setMainStatus({ status: "running" })
      mainOnTerm = onTerm
    }
    const daemons = await (
      await this.abi.main({
        effects: effects as MainEffects,
        started,
      })
    ).build()
    this.runningMain = {
      effects,
      stop: async () => {
        if (mainOnTerm) await mainOnTerm()
        await daemons.term()
      },
      callbacks: new CallbackHolder(),
    }
  }

  callCallback(callback: number, args: any[]): void {
    if (this.runningMain) {
      this.runningMain.callbacks
        .callCallback(callback, args)
        .catch((error) =>
          console.error(`callback ${callback} failed`, utils.asError(error)),
        )
    } else {
      console.warn(`callback ${callback} ignored because system is not running`)
    }
  }

  async stop(): Promise<void> {
    if (this.runningMain) {
      await this.runningMain.stop()
      await this.runningMain.effects.clearCallbacks()
      this.runningMain = undefined
    }
  }
}
