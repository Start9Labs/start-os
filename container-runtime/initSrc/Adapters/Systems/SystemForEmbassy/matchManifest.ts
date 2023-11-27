import {
  object,
  literal,
  string,
  array,
  boolean,
  dictionary,
  literals,
  number,
  unknown,
  some,
} from "ts-matches"
import { matchVolume } from "./matchVolume"

const matchDockerProcedure = object(
  {
    type: literal("docker"),
    image: string,
    system: boolean,
    entrypoint: string,
    args: array(string),
    mounts: dictionary([string, string]),
    "iso-format": literals(
      "json",
      "json-pretty",
      "yaml",
      "cbor",
      "toml",
      "toml-pretty",
    ),
    "sigterm-timeout": number,
  },
  ["iso-format"],
)
const matchJsProcedure = object({
  type: literal("script"),
  args: array(unknown),
})
const matchProcedure = some(matchDockerProcedure, matchJsProcedure)
export const matchManifest = object(
  {
    id: string,
    main: matchDockerProcedure,
    assets: object(
      {
        assets: string,
        scripts: string,
      },
      ["assets", "scripts"],
    ),
    "health-checks": dictionary([
      string,
      object({
        name: string,
        implementation: matchProcedure,
      }),
    ]),
    config: object({
      get: matchProcedure,
      set: matchProcedure,
    }),
    properties: matchProcedure,
    volumes: dictionary([string, matchVolume]),
    interfaces: dictionary([
      string,
      object({
        name: string,
        "tor-config": object({}),
        "lan-config": object({}),
        ui: boolean,
        protocols: array(string),
      }),
    ]),
    backup: object({
      create: matchProcedure,
      restore: matchProcedure,
    }),
    migrations: object({
      to: dictionary([string, matchProcedure]),
      from: dictionary([string, matchProcedure]),
    }),
    dependencies: dictionary([
      string,
      object(
        {
          version: string,
          requirement: some(
            object({
              type: literal("opt-in"),
              how: string,
            }),
            object({
              type: literal("opt-out"),
              how: string,
            }),
            object({
              type: literal("required"),
            }),
          ),
          description: string,
          config: object({
            check: matchProcedure,
            "auto-configure": matchProcedure,
          }),
        },
        ["description", "config"],
      ),
    ]),
  },
  ["config", "properties"],
)
export type Manifest = typeof matchManifest._TYPE
