import * as fs from "fs/promises"
import * as cp from "child_process"
import { Overlay, types as T } from "@start9labs/start-sdk"
import { promisify } from "util"
import { DockerProcedure, VolumeId } from "../../../Models/DockerProcedure"
import { Volume } from "./matchVolume"
export const exec = promisify(cp.exec)
export const execFile = promisify(cp.execFile)

export class DockerProcedureContainer {
  private constructor(readonly overlay: Overlay) {}
  // static async readonlyOf(data: DockerProcedure) {
  //   return DockerProcedureContainer.of(data, ["-o", "ro"])
  // }
  static async of(
    effects: T.Effects,
    packageId: string,
    data: DockerProcedure,
    volumes: { [id: VolumeId]: Volume },
  ) {
    const overlay = await Overlay.of(effects, { id: data.image })

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

    return new DockerProcedureContainer(overlay)
  }

  async exec(commands: string[], { destroy = true } = {}) {
    try {
      return await this.overlay.exec(commands)
    } finally {
      if (destroy) await this.overlay.destroy()
    }
  }

  async execFail(
    commands: string[],
    timeoutMs: number | null,
    { destroy = true } = {},
  ) {
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
      if (destroy) await this.overlay.destroy()
    }
  }

  async spawn(commands: string[]): Promise<cp.ChildProcessWithoutNullStreams> {
    return await this.overlay.spawn(commands)
  }
}
