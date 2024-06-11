import { ExecuteResult, System } from "../../Interfaces/System"
import { unNestPath } from "../../Models/JsonPath"
import { string } from "ts-matches"
import { HostSystemStartOs } from "../HostSystemStartOs"
import { Effects } from "../../Models/Effects"
import { RpcResult } from "../RpcListener"
import { duration } from "../../Models/Duration"
import { T } from "@start9labs/start-sdk"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"
export const STARTOS_JS_LOCATION = "/usr/lib/startos/package/index.js"
export class SystemForStartOs implements System {
  private onTerm: (() => Promise<void>) | undefined
  static of() {
    return new SystemForStartOs(require(STARTOS_JS_LOCATION))
  }
  constructor(readonly abi: T.ABI) {}
  async execute(
    effects: HostSystemStartOs,
    options: {
      procedure:
        | "/init"
        | "/uninit"
        | "/main/start"
        | "/main/stop"
        | "/config/set"
        | "/config/get"
        | "/backup/create"
        | "/backup/restore"
        | "/actions/metadata"
        | `/actions/${string}/get`
        | `/actions/${string}/run`
        | `/dependencies/${string}/query`
        | `/dependencies/${string}/update`
      input: unknown
      timeout?: number | undefined
    },
  ): Promise<RpcResult> {
    return { result: await this._execute(effects, options) }
  }
  async _execute(
    effects: Effects,
    options: {
      procedure:
        | "/init"
        | "/uninit"
        | "/main/start"
        | "/main/stop"
        | "/config/set"
        | "/config/get"
        | "/backup/create"
        | "/backup/restore"
        | "/actions/metadata"
        | `/actions/${string}/get`
        | `/actions/${string}/run`
        | `/dependencies/${string}/query`
        | `/dependencies/${string}/update`
      input: unknown
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
      case "/main/start": {
        const started = async (onTerm: () => Promise<void>) => {
          await effects.setMainStatus({ status: "running" })
          if (this.onTerm) await this.onTerm()
          this.onTerm = onTerm
        }
        const daemons = await (
          await this.abi.main({
            effects: { ...effects, _type: "main" },
            started,
          })
        ).build()
        this.onTerm = daemons.term
      }
      case "/main/stop": {
        await effects.setMainStatus({ status: "stopped" })
        if (this.onTerm) await this.onTerm()
        delete this.onTerm
        return duration(30, "s")
      }
      case "/config/set": {
        const input = options.input as any // TODO
        return this.abi.setConfig({ effects, input })
      }
      case "/config/get": {
        return this.abi.getConfig({ effects })
      }
      case "/backup/create":
      case "/backup/restore":
        throw new Error("this should be called with the init/unit")
      case "/actions/metadata": {
        return this.abi.actionsMetadata({ effects })
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
    }
    throw new Error(`Method ${options.procedure} not implemented.`)
  }
  async exit(effects: Effects): Promise<void> {
    return void null
  }
}
