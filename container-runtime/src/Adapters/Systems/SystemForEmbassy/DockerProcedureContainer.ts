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
    data: DockerProcedure,
    volumes: { [id: VolumeId]: Volume },
  ) {
    const overlay = await Overlay.of(effects, data.image)

    if (data.mounts) {
      const mounts = data.mounts
      for (const mount in mounts) {
        const path = mounts[mount].startsWith("/")
          ? `${overlay.rootfs}${mounts[mount]}`
          : `${overlay.rootfs}/${mounts[mount]}`
        await fs.mkdir(path, { recursive: true })
        const volumeMount = volumes[mount]
        if (volumeMount.type === "data") {
          await overlay.mount({ type: "volume", id: mount }, mounts[mount])
        } else if (volumeMount.type === "assets") {
          await overlay.mount({ type: "assets", id: mount }, mounts[mount])
        } else if (volumeMount.type === "certificate") {
          volumeMount
          const certChain = await effects.getSslCertificate({
            packageId: null,
            hostId: volumeMount["interface-id"],
            algorithm: null,
          })
          const key = await effects.getSslKey({
            packageId: null,
            hostId: volumeMount["interface-id"],
            algorithm: null,
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
          await effects.mount({
            location: path,
            target: {
              packageId: volumeMount["package-id"],
              path: volumeMount.path,
              readonly: volumeMount.readonly,
              volumeId: volumeMount["volume-id"],
            },
          })
        } else if (volumeMount.type === "backup") {
          throw new Error("TODO")
        }
      }
    }

    return new DockerProcedureContainer(overlay)
  }

  async exec(commands: string[]) {
    try {
      return await this.overlay.exec(commands)
    } finally {
      await this.overlay.destroy()
    }
  }

  async execSpawn(commands: string[]) {
    try {
      const spawned = await this.overlay.spawn(commands)
      return spawned
    } finally {
      await this.overlay.destroy()
    }
  }

  async spawn(commands: string[]): Promise<cp.ChildProcessWithoutNullStreams> {
    return await this.overlay.spawn(commands)
  }
}
