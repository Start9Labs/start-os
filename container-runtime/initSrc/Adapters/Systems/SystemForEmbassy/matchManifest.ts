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
  every,
} from "ts-matches"
import { matchVolume } from "./matchVolume"
import { matchDockerProcedure } from "../../../Models/DockerProcedure"

const matchJsProcedure = object(
  {
    type: literal("script"),
    args: array(unknown),
  },
  ["args"],
  {
    args: [],
  },
)

const matchProcedure = some(matchDockerProcedure, matchJsProcedure)
export type Procedure = typeof matchProcedure._TYPE

const matchAction = object(
  {
    name: string,
    description: string,
    warning: string,
    implementation: matchProcedure,
    "allowed-statuses": array(literals("running", "stopped")),
    "input-spec": unknown,
  },
  ["warning", "input-spec", "input-spec"],
)
export const matchManifest = object(
  {
    id: string,
    version: string,
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
      every(
        matchProcedure,
        object({
          name: string,
        }),
      ),
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

    actions: dictionary([string, matchAction]),
  },
  ["config", "actions", "properties", "migrations", "dependencies"],
)
export type Manifest = typeof matchManifest._TYPE
