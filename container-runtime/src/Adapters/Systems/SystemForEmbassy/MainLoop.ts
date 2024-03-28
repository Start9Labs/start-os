import { PolyfillEffects } from "./polyfillEffects"
import { DockerProcedureContainer } from "./DockerProcedureContainer"
import { SystemForEmbassy } from "."
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { Daemons, T, daemons } from "@start9labs/start-sdk"

const EMBASSY_HEALTH_INTERVAL = 15 * 1000
const EMBASSY_PROPERTIES_LOOP = 30 * 1000
/**
 * We wanted something to represent what the main loop is doing, and
 * in this case it used to run the properties, health, and the docker/ js main.
 * Also, this has an ability to clean itself up too if need be.
 */
export class MainLoop {
  private healthLoops:
    | {
        name: string
        interval: NodeJS.Timeout
      }[]
    | undefined

  private mainEvent:
    | Promise<{
        daemon: T.DaemonReturned
        wait: Promise<unknown>
      }>
    | undefined
  constructor(
    readonly system: SystemForEmbassy,
    readonly effects: HostSystemStartOs,
  ) {
    this.healthLoops = this.constructHealthLoops()
    this.mainEvent = this.constructMainEvent()
  }

  private async constructMainEvent() {
    const { system, effects } = this
    const currentCommand: [string, ...string[]] = [
      system.manifest.main.entrypoint,
      ...system.manifest.main.args,
    ]

    await effects.setMainStatus({ status: "running" })
    const jsMain = (this.system.moduleCode as any)?.jsMain
    const dockerProcedureContainer = await DockerProcedureContainer.of(
      effects,
      this.system.manifest.main,
      this.system.manifest.volumes,
    )
    if (jsMain) {
      const daemons = Daemons.of({
        effects,
        started: async (_) => {},
        healthReceipts: [],
      })
      throw new Error("todo")
      // return {
      //   daemon,
      //   wait: daemon.wait().finally(() => {
      //     this.clean()
      //     effects.setMainStatus({ status: "stopped" })
      //   }),
      // }
    }
    const daemon = await daemons.runDaemon()(
      this.effects,
      this.system.manifest.main.image,
      currentCommand,
      {
        overlay: dockerProcedureContainer.overlay,
      },
    )
    return {
      daemon,
      wait: daemon.wait().finally(() => {
        this.clean()
        effects
          .setMainStatus({ status: "stopped" })
          .catch((e) => console.error("Could not set the status to stopped"))
      }),
    }
  }

  public async clean(options?: { timeout?: number }) {
    const { mainEvent, healthLoops } = this
    const main = await mainEvent
    delete this.mainEvent
    delete this.healthLoops
    if (mainEvent) await main?.daemon.term()
    if (healthLoops) healthLoops.forEach((x) => clearInterval(x.interval))
  }

  private constructHealthLoops() {
    const { manifest } = this.system
    const effects = this.effects
    const start = Date.now()
    return Object.entries(manifest["health-checks"]).map(
      ([healthId, value]) => {
        const interval = setInterval(async () => {
          const actionProcedure = value
          const timeChanged = Date.now() - start
          if (actionProcedure.type === "docker") {
            const container = await DockerProcedureContainer.of(
              effects,
              actionProcedure,
              manifest.volumes,
            )
            const executed = await container.execSpawn([
              actionProcedure.entrypoint,
              ...actionProcedure.args,
              JSON.stringify(timeChanged),
            ])
            if (executed.exitCode === 59) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "disabled",
                message:
                  executed.stderr.toString() || executed.stdout.toString(),
              })
              return
            }
            if (executed.exitCode === 60) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "starting",
                message:
                  executed.stderr.toString() || executed.stdout.toString(),
              })
              return
            }
            if (executed.exitCode === 61) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "loading",
                message:
                  executed.stderr.toString() || executed.stdout.toString(),
              })
              return
            }
            const errorMessage = executed.stderr.toString()
            const message = executed.stdout.toString()
            if (!!errorMessage) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "failure",
                message: errorMessage,
              })
              return
            }
            await effects.setHealth({
              id: healthId,
              name: value.name,
              result: "success",
              message,
            })
            return
          } else {
            actionProcedure
            const moduleCode = await this.system.moduleCode
            const method = moduleCode.health?.[healthId]
            if (!method) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "failure",
                message: `Expecting that the js health check ${healthId} exists`,
              })
              return
            }

            const result = await method(
              new PolyfillEffects(effects, this.system.manifest),
              timeChanged,
            )

            if ("result" in result) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "success",
                message: null,
              })
              return
            }
            if ("error" in result) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "failure",
                message: result.error,
              })
              return
            }
            if (!("error-code" in result)) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "failure",
                message: `Unknown error type ${JSON.stringify(result)}`,
              })
              return
            }
            const [code, message] = result["error-code"]
            if (code === 59) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "disabled",
                message,
              })
              return
            }
            if (code === 60) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "starting",
                message,
              })
              return
            }
            if (code === 61) {
              await effects.setHealth({
                id: healthId,
                name: value.name,
                result: "loading",
                message,
              })
              return
            }

            await effects.setHealth({
              id: healthId,
              name: value.name,
              result: "failure",
              message: `${result["error-code"][0]}: ${result["error-code"][1]}`,
            })
            return
          }
        }, EMBASSY_HEALTH_INTERVAL)

        return { name: healthId, interval }
      },
    )
  }
}
