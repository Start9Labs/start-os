import { ExecuteResult, Procedure, System } from "../../Interfaces/System"
import { unNestPath } from "../../Models/JsonPath"
import matches, { any, number, object, string, tuple } from "ts-matches"
import { Effects } from "../../Models/Effects"
import { RpcResult, matchRpcResult } from "../RpcListener"
import { duration } from "../../Models/Duration"
import { T, utils } from "@start9labs/start-sdk"
import { Volume } from "../../Models/Volume"
import { CallbackHolder } from "../../Models/CallbackHolder"
import { Optional } from "ts-matches/lib/parsers/interfaces"

type MainEffects = T.MainEffects

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
    }))
  }
  async restoreBackup(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    return void (await this.abi.restoreBackup({
      effects,
    }))
  }
  properties(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<T.PropertiesReturn> {
    throw new Error("Method not implemented.")
  }
  getActionInput(
    effects: Effects,
    id: string,
    timeoutMs: number | null,
  ): Promise<T.ActionInput | null> {
    const action = this.abi.actions.get(id)
    if (!action) throw new Error(`Action ${id} not found`)
    return action.getInput({ effects })
  }
  runAction(
    effects: Effects,
    id: string,
    prev: T.ActionInput | null,
    input: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult | null> {
    const action = this.abi.actions.get(id)
    if (!action) throw new Error(`Action ${id} not found`)
    return action.run({ effects, input, prev: prev || undefined })
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
