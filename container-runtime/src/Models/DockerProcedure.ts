import {
  object,
  literal,
  string,
  boolean,
  array,
  dictionary,
  literals,
  number,
  Parser,
  some,
} from "ts-matches"
import { matchDuration } from "./Duration"

const VolumeId = string
const Path = string

export type VolumeId = string
export type Path = string
export const matchDockerProcedure = object(
  {
    type: literal("docker"),
    image: string,
    system: boolean,
    entrypoint: string,
    args: array(string),
    mounts: dictionary([VolumeId, Path]),
    "io-format": literals(
      "json",
      "json-pretty",
      "yaml",
      "cbor",
      "toml",
      "toml-pretty",
    ),
    "sigterm-timeout": some(number, matchDuration),
    inject: boolean,
  },
  ["io-format", "sigterm-timeout", "system", "args", "inject", "mounts"],
  {
    "sigterm-timeout": 30,
    inject: false,
    args: [],
  },
)

export type DockerProcedure = typeof matchDockerProcedure._TYPE
