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

const matchJsProcedure = object({
  type: literal("script"),
  args: array(unknown).nullable().optional().defaultTo([]),
})

const matchProcedure = some(matchDockerProcedure, matchJsProcedure)
export type Procedure = typeof matchProcedure._TYPE

const matchAction = object({
  name: string,
  description: string,
  warning: string.nullable().optional(),
  implementation: matchProcedure,
  "allowed-statuses": array(literals("running", "stopped")),
  "input-spec": unknown.nullable().optional(),
})
export const matchManifest = object({
  id: string,
  title: string,
  version: string,
  main: matchDockerProcedure,
  assets: object({
    assets: string.nullable().optional(),
    scripts: string.nullable().optional(),
  })
    .nullable()
    .optional(),
  "health-checks": dictionary([
    string,
    every(
      matchProcedure,
      object({
        name: string,
        ["success-message"]: string.nullable().optional(),
      }),
    ),
  ]),
  config: object({
    get: matchProcedure,
    set: matchProcedure,
  })
    .nullable()
    .optional(),
  properties: matchProcedure.nullable().optional(),
  volumes: dictionary([string, matchVolume]),
  interfaces: dictionary([
    string,
    object({
      name: string,
      description: string,
      "tor-config": object({
        "port-mapping": dictionary([string, string]),
      })
        .nullable()
        .optional(),
      "lan-config": dictionary([
        string,
        object({
          ssl: boolean,
          internal: number,
        }),
      ])
        .nullable()
        .optional(),
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
  })
    .nullable()
    .optional(),
  dependencies: dictionary([
    string,
    object({
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
      description: string.nullable().optional(),
      config: object({
        check: matchProcedure,
        "auto-configure": matchProcedure,
      })
        .nullable()
        .optional(),
    })
      .nullable()
      .optional(),
  ]),

  actions: dictionary([string, matchAction]),
})
export type Manifest = typeof matchManifest._TYPE
