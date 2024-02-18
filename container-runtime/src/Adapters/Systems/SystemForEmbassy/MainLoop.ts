import { PolyfillEffects } from "./polyfillEffects"
import { DockerProcedureContainer } from "./DockerProcedureContainer"
import { SystemForEmbassy } from "."
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { util, Daemons, types as T } from "@start9labs/start-sdk"

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
  private propertiesEvent: NodeJS.Timeout | undefined
  constructor(
    readonly system: SystemForEmbassy,
    readonly effects: HostSystemStartOs,
    readonly runProperties: () => Promise<void>,
  ) {
    this.healthLoops = this.constructHealthLoops()
    this.mainEvent = this.constructMainEvent()
    this.propertiesEvent = this.constructPropertiesEvent()
  }

  private async constructMainEvent() {
    const { system, effects } = this
    const utils = util.createUtils(effects)
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
    const daemon = await utils.runDaemon(
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
    const { mainEvent, healthLoops, propertiesEvent } = this
    delete this.mainEvent
    delete this.healthLoops
    delete this.propertiesEvent
    if (mainEvent) await (await mainEvent).daemon.term()
    clearInterval(propertiesEvent)
    if (healthLoops) healthLoops.forEach((x) => clearInterval(x.interval))
  }

  private constructPropertiesEvent() {
    const { runProperties } = this
    return setInterval(() => {
      runProperties()
    }, EMBASSY_PROPERTIES_LOOP)
  }

  private constructHealthLoops() {
    const { manifest } = this.system
    const effects = this.effects
    const start = Date.now()
    return Object.values(manifest["health-checks"]).map((value) => {
      const name = value.name
      const interval = setInterval(async () => {
        const actionProcedure = value
        const timeChanged = Date.now() - start
        if (actionProcedure.type === "docker") {
          const container = await DockerProcedureContainer.of(
            effects,
            actionProcedure,
            manifest.volumes,
          )
          const executed = await container.exec([
            actionProcedure.entrypoint,
            ...actionProcedure.args,
            JSON.stringify(timeChanged),
          ])
          const stderr = executed.stderr.toString()
          if (stderr)
            console.error(`Error running health check ${value.name}: ${stderr}`)
          return executed.stdout.toString()
        } else {
          const moduleCode = await this.system.moduleCode
          const method = moduleCode.health?.[value.name]
          if (!method)
            return console.error(
              `Expecting that thejs health check ${value.name} exists`,
            )
          return (await method(
            new PolyfillEffects(effects, this.system.manifest),
            timeChanged,
          ).then((x) => {
            if ("result" in x) return x.result
            if ("error" in x)
              return console.error("Error getting config: " + x.error)
            return console.error("Error getting config: " + x["error-code"][1])
          })) as any
        }
      }, EMBASSY_HEALTH_INTERVAL)

      return { name, interval }
    })
  }
}