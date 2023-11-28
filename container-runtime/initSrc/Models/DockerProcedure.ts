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

const VolumeId = string
const Path = string
export const matchDockerProcedure = object(
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

export type DockerProcedure = typeof matchDockerProcedure._TYPE
