import { ExecuteResult, System } from "../../Interfaces/System"
import { unNestPath } from "../../Models/JsonPath"
import { string } from "ts-matches"
import { HostSystemStartOs } from "../HostSystemStartOs"
import { Effects } from "../../Models/Effects"
const LOCATION = "/usr/lib/startos/package/startos"
export class SystemForStartOs implements System {
  private onTerm: (() => Promise<void>) | undefined
  static of() {
    return new SystemForStartOs()
  }
  constructor() {}
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
  ): Promise<ExecuteResult> {
    return { ok: await this._execute(effects, options) }
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
        const path = `${LOCATION}/procedures/init`
        const procedure: any = await import(path).catch(() => require(path))
        const previousVersion = string.optional().unsafeCast(options)
        return procedure.init({ effects, previousVersion })
      }
      case "/uninit": {
        const path = `${LOCATION}/procedures/init`
        const procedure: any = await import(path).catch(() => require(path))
        const nextVersion = string.optional().unsafeCast(options)
        return procedure.uninit({ effects, nextVersion })
      }
      case "/main/start": {
        const path = `${LOCATION}/procedures/main`
        const procedure: any = await import(path).catch(() => require(path))
        const started = async (onTerm: () => Promise<void>) => {
          await effects.setMainStatus({ status: "running" })
          if (this.onTerm) await this.onTerm()
          this.onTerm = onTerm
        }
        return procedure.main({ effects, started })
      }
      case "/main/stop": {
        await effects.setMainStatus({ status: "stopped" })
        if (this.onTerm) await this.onTerm()
        delete this.onTerm
        return
      }
      case "/config/set": {
        const path = `${LOCATION}/procedures/config`
        const procedure: any = await import(path).catch(() => require(path))
        const input = options.input
        return procedure.setConfig({ effects, input })
      }
      case "/config/get": {
        const path = `${LOCATION}/procedures/config`
        const procedure: any = await import(path).catch(() => require(path))
        return procedure.getConfig({ effects })
      }
      case "/backup/create":
      case "/backup/restore":
        throw new Error("this should be called with the init/unit")
      case "/actions/metadata": {
        const path = `${LOCATION}/procedures/actions`
        const procedure: any = await import(path).catch(() => require(path))
        return procedure.actionsMetadata({ effects })
      }
      default:
        const procedures = unNestPath(options.procedure)
        const id = procedures[2]
        switch (true) {
          case procedures[1] === "actions" && procedures[3] === "get": {
            const path = `${LOCATION}/procedures/actions`
            const action: any = (await import(path).catch(() => require(path)))
              .actions[id]
            if (!action) throw new Error(`Action ${id} not found`)
            return action.get({ effects })
          }
          case procedures[1] === "actions" && procedures[3] === "run": {
            const path = `${LOCATION}/procedures/actions`
            const action: any = (await import(path).catch(() => require(path)))
              .actions[id]
            if (!action) throw new Error(`Action ${id} not found`)
            const input = options.input
            return action.run({ effects, input })
          }
          case procedures[1] === "dependencies" && procedures[3] === "query": {
            const path = `${LOCATION}/procedures/dependencies`
            const dependencyConfig: any = (
              await import(path).catch(() => require(path))
            ).dependencyConfig[id]
            if (!dependencyConfig)
              throw new Error(`dependencyConfig ${id} not found`)
            const localConfig = options.input
            return dependencyConfig.query({ effects, localConfig })
          }
          case procedures[1] === "dependencies" && procedures[3] === "update": {
            const path = `${LOCATION}/procedures/dependencies`
            const dependencyConfig: any = (
              await import(path).catch(() => require(path))
            ).dependencyConfig[id]
            if (!dependencyConfig)
              throw new Error(`dependencyConfig ${id} not found`)
            return dependencyConfig.update(options.input)
          }
        }
    }
    throw new Error("Method not implemented.")
  }
  exit(effects: Effects): Promise<void> {
    throw new Error("Method not implemented.")
  }
}
