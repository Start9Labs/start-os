import * as fs from "fs/promises"
import * as cp from "child_process"
import { SubContainer, types as T } from "@start9labs/start-sdk"
import { promisify } from "util"
import { DockerProcedure, VolumeId } from "../../../Models/DockerProcedure"
import { Volume } from "./matchVolume"
import { ExecSpawnable } from "@start9labs/start-sdk/cjs/lib/util/Overlay"
export const exec = promisify(cp.exec)
export const execFile = promisify(cp.execFile)

export class DockerProcedureContainer {
  private constructor(private readonly overlay: ExecSpawnable) {}

  static async of(
    effects: T.Effects,
    packageId: string,
    data: DockerProcedure,
    volumes: { [id: VolumeId]: Volume },
    options: { overlay?: ExecSpawnable } = {},
  ) {
    const overlay =
      options?.overlay ??
      (await DockerProcedureContainer.createOverlay(
        effects,
        packageId,
        data,
        volumes,
      ))
    return new DockerProcedureContainer(overlay)
  }
  static async createOverlay(
    effects: T.Effects,
    packageId: string,
    data: DockerProcedure,
    volumes: { [id: VolumeId]: Volume },
  ) {
    const overlay = await SubContainer.of(effects, { id: data.image })

    if (data.mounts) {
      const mounts = data.mounts
      for (const mount in mounts) {
        const path = mounts[mount].startsWith("/")
          ? `${overlay.rootfs}${mounts[mount]}`
          : `${overlay.rootfs}/${mounts[mount]}`
        await fs.mkdir(path, { recursive: true })
        const volumeMount = volumes[mount]
        if (volumeMount.type === "data") {
          await overlay.mount(
            { type: "volume", id: mount, subpath: null, readonly: false },
            mounts[mount],
          )
        } else if (volumeMount.type === "assets") {
          await overlay.mount(
            { type: "assets", id: mount, subpath: null },
            mounts[mount],
          )
        } else if (volumeMount.type === "certificate") {
          const hostnames = [
            `${packageId}.embassy`,
            ...new Set(
              Object.values(
                (
                  await effects.getHostInfo({
                    hostId: volumeMount["interface-id"],
                  })
                )?.hostnameInfo || {},
              )
                .flatMap((h) => h)
                .flatMap((h) => (h.kind === "onion" ? [h.hostname.value] : [])),
            ).values(),
          ]
          const certChain = await effects.getSslCertificate({
            hostnames,
          })
          const key = await effects.getSslKey({
            hostnames,
          })
          await fs.writeFile(
            `${path}/${volumeMount["interface-id"]}.cert.pem`,
            certChain.join("\n"),
          )
          await fs.writeFile(
            `${path}/${volumeMount["interface-id"]}.key.pem`,
            key,
          )
        } else if (volumeMount.type === "pointer") {
          await effects
            .mount({
              location: path,
              target: {
                packageId: volumeMount["package-id"],
                subpath: volumeMount.path,
                readonly: volumeMount.readonly,
                volumeId: volumeMount["volume-id"],
              },
            })
            .catch(console.warn)
        } else if (volumeMount.type === "backup") {
          await overlay.mount({ type: "backup", subpath: null }, mounts[mount])
        }
      }
    }
    return overlay
  }

  async exec(commands: string[], {} = {}) {
    try {
      return await this.overlay.exec(commands)
    } finally {
      await this.overlay.destroy?.()
    }
  }

  async execFail(commands: string[], timeoutMs: number | null, {} = {}) {
    try {
      const res = await this.overlay.exec(commands, {}, timeoutMs)
      if (res.exitCode !== 0) {
        const codeOrSignal =
          res.exitCode !== null
            ? `code ${res.exitCode}`
            : `signal ${res.exitSignal}`
        throw new Error(
          `Process exited with ${codeOrSignal}: ${res.stderr.toString()}`,
        )
      }
      return res
    } finally {
      await this.overlay.destroy?.()
    }
  }

  async spawn(commands: string[]): Promise<cp.ChildProcessWithoutNullStreams> {
    return await this.overlay.spawn(commands)
  }
}
