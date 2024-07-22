import { ExecuteResult, System } from "../../Interfaces/System"
import { unNestPath } from "../../Models/JsonPath"
import matches, {
  any,
  dictionary,
  number,
  object,
  string,
  unknown,
  tuple,
} from "ts-matches"
import { hostSystemStartOs } from "../HostSystemStartOs"
import { Effects } from "../../Models/Effects"
import { RpcResult, matchRpcResult } from "../RpcListener"
import { duration } from "../../Models/Duration"
import { T } from "@start9labs/start-sdk"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"
import { Volume } from "../../Models/Volume"
import { Optional } from "ts-matches/lib/parsers/interfaces"
import { ActionMetadata } from "@start9labs/start-sdk/cjs/lib/osBindings"
export const STARTOS_JS_LOCATION = "/usr/lib/startos/package/index.js"

const matchRecord = dictionary([string, unknown])
export class SystemForStartOs implements System {
  private onTerm: (() => Promise<void>) | undefined
  static of() {
    return new SystemForStartOs(require(STARTOS_JS_LOCATION))
  }
  constructor(readonly abi: T.ABI) {}
  async mainStart(effects: T.Effects, timeoutMs: number | null): Promise<void> {
    if (this.onTerm) await this.onTerm()
    const started = async (onTerm: () => Promise<void>) => {
      await effects.setMainStatus({ status: "running" })
      this.onTerm = onTerm
    }
    const daemons = await (
      await this.abi.main({
        effects: { ...effects, _type: "main" },
        started,
      })
    ).build()
    this.onTerm = daemons.term
    return
  }
  async mainStop(effects: T.Effects, timeoutMs: number | null): Promise<void> {
    try {
      if (this.onTerm) await this.onTerm()
      delete this.onTerm
      return
    } finally {
      await effects.setMainStatus({ status: "stopped" })
    }
  }
  async init(
    effects: T.Effects,
    previousVersion: Optional<string> = null,
    timeoutMs: number | null = null,
  ): Promise<void> {
    const _: unknown = await this.abi.init({ effects, previousVersion })
    return
  }
  async uninit(
    effects: T.Effects,
    nextVersion: Optional<string> = null,
    timeoutMs: number | null = null,
  ): Promise<void> {
    const _: unknown = await this.abi.uninit({ effects, nextVersion })
    return
  }
  async createBackup(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    const _: unknown = await this.abi.createBackup({
      effects,
      pathMaker: ((options) =>
        new Volume(options.volume, options.path).path) as T.PathMaker,
    })
    return
  }
  async restoreBackup(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<void> {
    const _: unknown = await this.abi.restoreBackup({
      effects,
      pathMaker: ((options) =>
        new Volume(options.volume, options.path).path) as T.PathMaker,
    })
    return
  }
  async getConfig(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<T.ConfigRes> {
    return this.abi.getConfig({ effects })
  }
  async setConfig(
    effects: T.Effects,
    input: Record<string, unknown>,
    timeoutMs: number | null,
  ): Promise<void> {
    return this.abi.setConfig({ effects, input })
  }
  async migration(
    effects: T.Effects,
    fromVersion: string,
    timeoutMs: number | null,
  ): Promise<T.MigrationRes> {
    throw new Error("Method not implemented.")
  }
  async properties(
    effects: T.Effects,
    timeoutMs: number | null,
  ): Promise<T.PropertiesReturn> {
    return this.abi.properties({ effects })
  }
  async action(
    effects: T.Effects,
    actionId: string,
    formData: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult> {
    const action = (await this.abi.actions({ effects }))[actionId]
    if (!action) throw new Error(`Action ${actionId} not found`)
    const input = matchRecord.unsafeCast(formData)
    return action.run({ effects, input: input }) // TODO
  }
  dependenciesCheck(
    effects: T.Effects,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<any> {
    const dependencyConfig = this.abi.dependencyConfig[id]
    if (!dependencyConfig) throw new Error(`dependencyConfig ${id} not found`)
    // const localConfig = options.input
    return dependencyConfig.query({ effects })
  }
  async dependenciesAutoconfig(
    effects: T.Effects,
    id: string,
    remoteConfig: unknown,
    timeoutMs: number | null,
  ): Promise<void> {
    const dependencyConfig = this.abi.dependencyConfig[id]
    if (!dependencyConfig) throw new Error(`dependencyConfig ${id} not found`)
    const queryResults = await this.getConfig(effects, timeoutMs)
    const _: unknown = dependencyConfig.update({
      remoteConfig,
      queryResults,
    }) // TODO
    return
  }
  dependenciesMetadata(effects: T.Effects): Promise<ActionMetadata[]> {
    return this.abi.actionsMetadata({ effects })
  }

  // async _execute(
  //   effects: Effects,
  //   options: {
  //     procedure:
  //       | "/init"
  //       | "/uninit"
  //       | "/main/start"
  //       | "/main/stop"
  //       | "/config/set"
  //       | "/config/get"
  //       | "/backup/create"
  //       | "/backup/restore"
  //       | "/actions/metadata"
  //       | `/actions/${string}/get`
  //       | `/actions/${string}/run`
  //       | `/dependencies/${string}/query`
  //       | `/dependencies/${string}/update`
  //     input: unknown
  //     timeout?: number | undefined
  //   },
  // ): Promise<unknown> {
  //   switch (options.procedure) {
  //     default:
  //       const procedures = unNestPath(options.procedure)
  //       const id = procedures[2]
  //       switch (true) {
  //         case procedures[1] === "actions" && procedures[3] === "get": {
  //           const action = (await this.abi.actions({ effects }))[id]
  //           if (!action) throw new Error(`Action ${id} not found`)
  //           return action.getConfig({ effects })
  //         }
  //         case procedures[1] === "actions" && procedures[3] === "run": {
  //         }
  //         case procedures[1] === "dependencies" && procedures[3] === "query": {
  //         }
  //         case procedures[1] === "dependencies" && procedures[3] === "update": {
  //         }
  //       }
  //       return
  //   }
  //   throw new Error(`Method ${options.procedure} not implemented.`)
  // }
  async exit(effects: Effects): Promise<void> {
    return void null
  }
}
