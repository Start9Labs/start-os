import {
  object,
  literal,
  string,
  boolean,
  array,
  dictionary,
  literals,
  number,
} from "ts-matches"
import fs from "fs/promises"
import cp from "child_process"
import { promisify } from "util"
const exec = promisify(cp.exec)

const VolumeId = string
const Path = string
const _matchDockerProcedure = object(
  {
    type: literal("docker"),
    image: string,
    system: boolean,
    entrypoint: string,
    args: array(string),
    mounts: dictionary([VolumeId, Path]),
    "iso-format": literals(
      "json",
      "json-pretty",
      "yaml",
      "cbor",
      "toml",
      "toml-pretty",
    ),
    "sigterm-timeout": number,
    inject: boolean,
  },
  ["iso-format"],
)

type MatchDockerProcedure = typeof _matchDockerProcedure._TYPE

export class DockerProcedure implements Readonly<MatchDockerProcedure> {
  constructor(readonly data: MatchDockerProcedure) {}
  get type() {
    return this.data.type
  }

  get image() {
    return this.data.image
  }
  get system() {
    return this.data.system
  }
  get entrypoint() {
    return this.data.entrypoint
  }
  get args() {
    return this.data.args
  }
  get mounts() {
    return this.data.mounts
  }
  get "iso-format"() {
    return this.data["iso-format"]
  }
  get "sigterm-timeout"() {
    return this.data["sigterm-timeout"]
  }

  get inject() {
    return this.data.inject
  }

  async makeContainer() {
    const image = this.data.image
    if (await fs.stat(`/media/images/${this.data.image}`).catch(() => false))
      throw new Error(`Image ${this.data.image} does not exist`)
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
    return {
      rootfs,
      async [Symbol.asyncDispose]() {
        await exec(`umount --recursive ${rootfs}`)
        rootfs && (await fs.rmdir(rootfs))
      },
    }
  }
}

export const matchDockerProcedure = _matchDockerProcedure.map(
  (x) => new DockerProcedure(x),
)
