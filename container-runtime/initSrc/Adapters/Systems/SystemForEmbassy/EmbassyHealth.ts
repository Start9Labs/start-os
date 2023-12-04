import * as T from "@start9labs/start-sdk/lib/types";
import { PolyfillEffects } from './polyfillEffects';
import { DockerProcedureContainer } from './DockerProcedureContainer';
import { SystemForEmbassy } from ".";

const EMBASSY_HEALTH_INTERVAL = 15 * 1000;
export class EmbassyHealth {
  constructor(readonly system: SystemForEmbassy, readonly effects: T.Effects) { }
  private healthLoops = this.constructHealthLoops();

  private constructHealthLoops() {
    const { manifest } = this.system;
    const effects = this.effects;
    const start = Date.now();
    return Object.values(manifest["health-checks"])
      .map((value) => {
        const name = value.name;
        const interval = setInterval(async () => {
          const actionProcedure = value.implementation;
          const timeChanged = Date.now() - start;
          if (actionProcedure.type === "docker") {
            await using container = await DockerProcedureContainer.of(actionProcedure);
            const stderr = (await container.exec([actionProcedure.entrypoint, ...actionProcedure.args, JSON.stringify(timeChanged)])).stderr;
            if (stderr) console.error(`Error running health check ${value.name}: ${stderr}`);
          } else {
            const moduleCode = await this.system.moduleCode;
            const method = moduleCode.health?.[value.name];
            if (!method) return console.error(`Expecting that thejs health check ${value.name} exists`);
            return await method(new PolyfillEffects(effects), timeChanged).then(x => {
              if ('result' in x) return x.result;
              if ('error' in x) return console.error("Error getting config: " + x.error);
              return console.error("Error getting config: " + x['error-code'][1]);
            }) as any;
          }
        }, EMBASSY_HEALTH_INTERVAL);

        return { name, interval };
      });
  }

  [Symbol.dispose]() {
    Object.values(this.healthLoops).forEach((x) => clearInterval(x.interval));
  }
}
