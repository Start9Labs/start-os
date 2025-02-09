import { System } from "../../Interfaces/System"
import { Effects } from "../../Models/Effects"
import { T, utils } from "@start9labs/start-sdk"
import { Optional } from "ts-matches/lib/parsers/interfaces"

export const STARTOS_JS_LOCATION = "/usr/lib/startos/package/index.js"

type RunningMain = {
  stop: () => Promise<void>
}

export class SystemForStartOs implements System {
  private runningMain: RunningMain | undefined

  static of() {
    return new SystemForStartOs(require(STARTOS_JS_LOCATION))
  }

  constructor(readonly abi: T.ABI) {
    this
  }
  async containerInit(effects: Effects): Promise<void> {
    return void (await this.abi.containerInit({ effects }))
  }
  async packageInit(
    effects: Effects,
    timeoutMs: number | null = null,
  ): Promise<void> {
    return void (await this.abi.packageInit({ effects }))
  }
  async packageUninit(
    effects: Effects,
    nextVersion: Optional<string> = null,
    timeoutMs: number | null = null,
  ): Promise<void> {
    return void (await this.abi.packageUninit({ effects, nextVersion }))
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
    input: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult | null> {
    const action = this.abi.actions.get(id)
    if (!action) throw new Error(`Action ${id} not found`)
    return action.run({ effects, input })
  }

  async exit(): Promise<void> {}

  async start(effects: Effects): Promise<void> {
    if (this.runningMain) return
    effects.constRetry = utils.once(() => effects.restart())
    let mainOnTerm: () => Promise<void> | undefined
    const started = async (onTerm: () => Promise<void>) => {
      await effects.setMainStatus({ status: "running" })
      mainOnTerm = onTerm
      return null
    }
    const daemons = await (
      await this.abi.main({
        effects,
        started,
      })
    ).build()
    this.runningMain = {
      stop: async () => {
        if (mainOnTerm) await mainOnTerm()
        await daemons.term()
      },
    }
  }

  async stop(): Promise<void> {
    if (this.runningMain) {
      try {
        await this.runningMain.stop()
      } finally {
        this.runningMain = undefined
      }
    }
  }
}
