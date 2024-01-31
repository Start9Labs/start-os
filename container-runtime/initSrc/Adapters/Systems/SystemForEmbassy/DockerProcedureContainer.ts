import fs from "fs/promises"
import * as T from "@start9labs/start-sdk/lib/types"
import cp from "child_process"
import { promisify } from "util"
import { DockerProcedure, VolumeId } from "../../../Models/DockerProcedure"
import { Volume } from "./matchVolume"
export const exec = promisify(cp.exec)
export const execFile = promisify(cp.execFile)

export class DockerProcedureContainer {
  private constructor(readonly rootfs: string) {}
  // static async readonlyOf(data: DockerProcedure) {
  //   return DockerProcedureContainer.of(data, ["-o", "ro"])
  // }
  static async of(
    effects: T.Effects,
    data: DockerProcedure,
    volumes: { [id: VolumeId]: Volume },
  ) {
    const imageId = data.image
    const rootfs = await effects.createOverlayedImage({ imageId })

    for (const dirPart of ["dev", "sys", "proc", "run"] as const) {
      const dir = await fs.mkdir(`${rootfs}/${dirPart}`, { recursive: true })
      if (!dir) break
      await execFile("mount", ["--bind", `/${dirPart}`, dir])
    }
    if (data.mounts) {
      const mounts = data.mounts
      for (const mount in mounts) {
        const path = mounts[mount].startsWith("/")
          ? `${rootfs}${mounts[mount]}`
          : `${rootfs}/${mounts[mount]}`
        await fs.mkdir(path, { recursive: true })
        const volumeMount = volumes[mount]
        if (volumeMount.type === "data") {
          await execFile("mount", [
            "--bind",
            `/media/startos/volumes/${mount}`,
            path,
          ])
        } else if (volumeMount.type === "assets") {
          await execFile("mount", [
            "--bind",
            `/media/startos/assets/${mount}`,
            path,
          ])
        } else if (volumeMount.type === "certificate") {
          const certChain = await effects.getSslCertificate()
          const key = await effects.getSslKey()
          await fs.writeFile(
            `${path}/${volumeMount["interface-id"]}.cert.pem`,
            certChain.join("\n"),
          )
          await fs.writeFile(
            `${path}/${volumeMount["interface-id"]}.key.pem`,
            key,
          )
        } else if (volumeMount.type === "pointer") {
          effects.mount({
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

    return new DockerProcedureContainer(rootfs)
  }

  async exec(commands: string[]) {
    try {
      return await execFile("chroot", [this.rootfs, ...commands])
    } finally {
      await exec(`umount --recursive ${this.rootfs}`)
      await fs.rm(this.rootfs, { recursive: true, force: true })
    }
  }
}
