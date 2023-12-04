import fs from "fs/promises"

import cp from "child_process"
import { promisify } from "util"
import { DockerProcedure } from "../../../Models/DockerProcedure"
export const exec = promisify(cp.exec)
export const execFile = promisify(cp.execFile)

export class DockerProcedureContainer {
  private constructor(readonly rootfs: string) {}
  static async readonlyOf(data: DockerProcedure) {
    return DockerProcedureContainer.of(data, ["-o", "ro"])
  }
  static async of(data: DockerProcedure, mountArgs: string[] = []) {
    const image = data.image
    if (await fs.stat(`/media/images/${data.image}`).catch(() => false))
      throw new Error(`Image ${data.image} does not exist`)
    const container = await fs.mkdtemp(`dockerProcedureTmp`)
    const rootfs = await fs.mkdir(`${container}/rootfs`, { recursive: true })
    const upper = await fs.mkdir(`${container}/upper`, { recursive: true })
    const work = await fs.mkdir(`${container}/work`, { recursive: true })

    if (!rootfs) {
      throw new Error(`Failed to create rootfs`)
    }

    await execFile("mount", [
      "-t",
      "overlay",
      ...mountArgs,
      `-olowerdir=/media/images/${image},upper=${upper},workdir=${work}`,
      "overlay",
      rootfs,
    ])

    for (const dirPart of ["dev", "sys", "proc", "run"] as const) {
      const dir = await fs.mkdir(`${rootfs}/${dirPart}`, { recursive: true })
      if (!dir) break
      await execFile("mount", [...mountArgs, "--bind", `/${dirPart}`, dir])
    }

    return new DockerProcedureContainer(rootfs)
  }
  async [Symbol.asyncDispose]() {
    await exec(`umount --recursive ${this.rootfs}`)
    await fs.rm(this.rootfs, { recursive: true, force: true })
  }

  async exec(commands: string[]) {
    return await execFile("chroot", [this.rootfs, ...commands])
  }
}
