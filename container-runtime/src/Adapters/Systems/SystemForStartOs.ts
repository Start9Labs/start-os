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

  async execute(
    effects: Effects,
    options: {
      procedure: Procedure
      input?: unknown
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
  async _execute(
    effects: Effects | MainEffects,
    options: {
      procedure: Procedure
      input?: unknown
      timeout?: number | undefined
    },
  ): Promise<unknown> {
    switch (options.procedure) {
      case "/init": {
        const previousVersion =
          string.optional().unsafeCast(options.input) || null
        return this.abi.init({ effects, previousVersion })
      }
      case "/uninit": {
        const nextVersion = string.optional().unsafeCast(options.input) || null
        return this.abi.uninit({ effects, nextVersion })
      }
      // case "/main/start": {
      //
      // }
      // case "/main/stop": {
      //   if (this.onTerm) await this.onTerm()
      //   await effects.setMainStatus({ status: "stopped" })
      //   delete this.onTerm
      //   return duration(30, "s")
      // }
      case "/config/set": {
        const input = options.input as any // TODO
        return this.abi.setConfig({ effects, input })
      }
      case "/config/get": {
        return this.abi.getConfig({ effects })
      }
      case "/backup/create":
        return this.abi.createBackup({
          effects,
          pathMaker: ((options) =>
            new Volume(options.volume, options.path).path) as T.PathMaker,
        })
      case "/backup/restore":
        return this.abi.restoreBackup({
          effects,
          pathMaker: ((options) =>
            new Volume(options.volume, options.path).path) as T.PathMaker,
        })
      case "/actions/metadata": {
        return this.abi.actionsMetadata({ effects })
      }
      case "/properties": {
        throw new Error("TODO")
      }
      default:
        const procedures = unNestPath(options.procedure)
        const id = procedures[2]
        switch (true) {
          case procedures[1] === "actions" && procedures[3] === "get": {
            const action = (await this.abi.actions({ effects }))[id]
            if (!action) throw new Error(`Action ${id} not found`)
            return action.getConfig({ effects })
          }
          case procedures[1] === "actions" && procedures[3] === "run": {
            const action = (await this.abi.actions({ effects }))[id]
            if (!action) throw new Error(`Action ${id} not found`)
            return action.run({ effects, input: options.input as any }) // TODO
          }
          case procedures[1] === "dependencies" && procedures[3] === "query": {
            const dependencyConfig = this.abi.dependencyConfig[id]
            if (!dependencyConfig)
              throw new Error(`dependencyConfig ${id} not found`)
            const localConfig = options.input
            return dependencyConfig.query({ effects })
          }
          case procedures[1] === "dependencies" && procedures[3] === "update": {
            const dependencyConfig = this.abi.dependencyConfig[id]
            if (!dependencyConfig)
              throw new Error(`dependencyConfig ${id} not found`)
            return dependencyConfig.update(options.input as any) // TODO
          }
        }
        return
    }
  }

  async sandbox(
    effects: Effects,
    options: { procedure: Procedure; input?: unknown; timeout?: number },
  ): Promise<RpcResult> {
    return this.execute(effects, options)
  }
}
