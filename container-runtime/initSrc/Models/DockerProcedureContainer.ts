import fs from "fs/promises"

import cp from "child_process"
import { promisify } from "util"
import { DockerProcedure } from "./DockerProcedure"
export const exec = promisify(cp.exec)

export class DockerProcedureContainer {
  private constructor(readonly rootfs: string | undefined) {}
  static async of(data: DockerProcedure) {
    const image = data.image
    if (await fs.stat(`/media/images/${data.image}`).catch(() => false))
      throw new Error(`Image ${data.image} does not exist`)
    const container = await fs.mkdtemp(`dockerProcedureTmp`)
    const rootfs = await fs.mkdir(`${container}/rootfs`, { recursive: true })
    const upper = await fs.mkdir(`${container}/upper`, { recursive: true })
    const work = await fs.mkdir(`${container}/work`, { recursive: true })

    await exec(
      `mount -t overlay -olowerdir=/media/images/${image},upperdir=${upper},workdir=${work} overlay ${rootfs}`,
    )

    for (const dirPart of ["dev", "sys", "proc", "run"] as const) {
      const dir = await fs.mkdir(`${rootfs}/${dirPart}`, { recursive: true })
      await exec(`mount --bind /${dirPart} ${dir}`)
    }

    return new DockerProcedureContainer(rootfs)
  }
  async [Symbol.asyncDispose]() {
    await exec(`umount --recursive ${this.rootfs}`)
    this.rootfs && (await fs.rmdir(this.rootfs))
  }
}
