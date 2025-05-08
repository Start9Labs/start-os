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
export const matchDockerProcedure = object({
  type: literal("docker"),
  image: string,
  system: boolean.optional(),
  entrypoint: string,
  args: array(string).defaultTo([]),
  mounts: dictionary([VolumeId, Path]).optional(),
  "io-format": literals(
    "json",
    "json-pretty",
    "yaml",
    "cbor",
    "toml",
    "toml-pretty",
  ).optional(),
  "sigterm-timeout": some(number, matchDuration).defaultTo(30),
  inject: boolean.defaultTo(false),
})

export type DockerProcedure = typeof matchDockerProcedure._TYPE
