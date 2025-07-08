// @ts-check

import * as net from "net"
import {
  object,
  some,
  string,
  literal,
  array,
  number,
  matches,
  any,
  shape,
  anyOf,
  literals,
} from "ts-matches"

import {
  ExtendedVersion,
  types as T,
  utils,
  VersionRange,
} from "@start9labs/start-sdk"
import * as fs from "fs"

import { CallbackHolder } from "../Models/CallbackHolder"
import { AllGetDependencies } from "../Interfaces/AllGetDependencies"
import { jsonPath, unNestPath } from "../Models/JsonPath"
import { System } from "../Interfaces/System"
import { makeEffects } from "./EffectCreator"
type MaybePromise<T> = T | Promise<T>
export const matchRpcResult = anyOf(
  object({ result: any }),
  object({
    error: object({
      code: number,
      message: string,
      data: object({
        details: string.optional(),
        debug: any.optional(),
      })
        .nullable()
        .optional(),
    }),
  }),
)

export type RpcResult = typeof matchRpcResult._TYPE
type SocketResponse = ({ jsonrpc: "2.0"; id: IdType } & RpcResult) | null

const SOCKET_PARENT = "/media/startos/rpc"
const SOCKET_PATH = "/media/startos/rpc/service.sock"
const jsonrpc = "2.0" as const

const isResult = object({ result: any }).test

const idType = some(string, number, literal(null))
type IdType = null | string | number | undefined
const runType = object({
  id: idType.optional(),
  method: literal("execute"),
  params: object({
    id: string,
    procedure: string,
    input: any,
    timeout: number.nullable().optional(),
  }),
})
const sandboxRunType = object({
  id: idType.optional(),
  method: literal("sandbox"),
  params: object({
    id: string,
    procedure: string,
    input: any,
    timeout: number.nullable().optional(),
  }),
})
const callbackType = object({
  method: literal("callback"),
  params: object({
    id: number,
    args: array,
  }),
})
const initType = object({
  id: idType.optional(),
  method: literal("init"),
  params: object({
    id: string,
    kind: literals("install", "update", "restore").nullable(),
  }),
})
const startType = object({
  id: idType.optional(),
  method: literal("start"),
})
const stopType = object({
  id: idType.optional(),
  method: literal("stop"),
})
const exitType = object({
  id: idType.optional(),
  method: literal("exit"),
  params: object({
    id: string,
    target: string.nullable(),
  }),
})
const evalType = object({
  id: idType.optional(),
  method: literal("eval"),
  params: object({
    script: string,
  }),
})

const jsonParse = (x: string) => JSON.parse(x)

const handleRpc = (id: IdType, result: Promise<RpcResult>) =>
  result
    .then((result) => {
      return {
        jsonrpc,
        id,
        ...result,
      }
    })
    .then((x) => {
      if (
        ("result" in x && x.result === undefined) ||
        !("error" in x || "result" in x)
      )
        (x as any).result = null
      return x
    })
    .catch((error) => ({
      jsonrpc,
      id,
      error: {
        code: 0,
        message: typeof error,
        data: { details: "" + error, debug: error?.stack },
      },
    }))

const hasId = object({ id: idType }).test
export class RpcListener {
  unixSocketServer = net.createServer(async (server) => {})
  private _system: System | undefined
  private callbacks: CallbackHolder | undefined

  constructor(readonly getDependencies: AllGetDependencies) {
    if (!fs.existsSync(SOCKET_PARENT)) {
      fs.mkdirSync(SOCKET_PARENT, { recursive: true })
    }
    if (fs.existsSync(SOCKET_PATH)) fs.rmSync(SOCKET_PATH, { force: true })

    this.unixSocketServer.listen(SOCKET_PATH)

    this.unixSocketServer.on("connection", (s) => {
      let id: IdType = null
      const captureId = <X>(x: X) => {
        if (hasId(x)) id = x.id
        return x
      }
      const logData =
        (location: string) =>
        <X>(x: X) => {
          console.log({
            location,
            stringified: JSON.stringify(x),
            type: typeof x,
            id,
          })
          return x
        }
      const mapError = (error: any): SocketResponse => ({
        jsonrpc,
        id,
        error: {
          message: typeof error,
          data: {
            details: error?.message ?? String(error),
            debug: error?.stack,
          },
          code: 1,
        },
      })
      const writeDataToSocket = (x: SocketResponse) => {
        if (x != null) {
          return new Promise((resolve) =>
            s.write(JSON.stringify(x) + "\n", resolve),
          )
        }
      }
      s.on("data", (a) =>
        Promise.resolve(a)
          .then((b) => b.toString())
          .then((buf) => {
            for (let s of buf.split("\n")) {
              if (s)
                Promise.resolve(s)
                  .then(logData("dataIn"))
                  .then(jsonParse)
                  .then(captureId)
                  .then((x) => this.dealWithInput(x))
                  .catch(mapError)
                  .then(logData("response"))
                  .then(writeDataToSocket)
                  .catch((e) => {
                    console.error(`Major error in socket handling: ${e}`)
                    console.debug(`Data in: ${a.toString()}`)
                  })
            }
          }),
      )
    })
  }

  private get system() {
    if (!this._system) throw new Error("System not initialized")
    return this._system
  }

  callCallback(callback: number, args: any[]): void {
    if (this.callbacks) {
      this.callbacks
        .callCallback(callback, args)
        .catch((error) =>
          console.error(`callback ${callback} failed`, utils.asError(error)),
        )
    } else {
      console.warn(
        `callback ${callback} ignored because system is not initialized`,
      )
    }
  }

  private dealWithInput(input: unknown): MaybePromise<SocketResponse> {
    return matches(input)
      .when(runType, async ({ id, params }) => {
        const system = this.system
        const procedure = jsonPath.unsafeCast(params.procedure)
        const { input, timeout, id: procedureId } = params
        const result = this.getResult(
          procedure,
          system,
          procedureId,
          timeout,
          input,
        )

        return handleRpc(id, result)
      })
      .when(sandboxRunType, async ({ id, params }) => {
        const system = this.system
        const procedure = jsonPath.unsafeCast(params.procedure)
        const { input, timeout, id: procedureId } = params
        const result = this.getResult(
          procedure,
          system,
          procedureId,
          timeout,
          input,
        )

        return handleRpc(id, result)
      })
      .when(callbackType, async ({ params: { id, args } }) => {
        this.callCallback(id, args)
        return null
      })
      .when(startType, async ({ id }) => {
        const callbacks =
          this.callbacks?.getChild("main") || this.callbacks?.child("main")
        const effects = makeEffects({
          procedureId: null,
          callbacks,
        })
        return handleRpc(
          id,
          this.system.start(effects).then((result) => ({ result })),
        )
      })
      .when(stopType, async ({ id }) => {
        this.callbacks?.removeChild("main")
        return handleRpc(
          id,
          this.system.stop().then((result) => ({ result })),
        )
      })
      .when(exitType, async ({ id, params }) => {
        return handleRpc(
          id,
          (async () => {
            if (this._system) {
              let target = null
              if (params.target)
                try {
                  target = ExtendedVersion.parse(params.target)
                } catch (_) {
                  target = VersionRange.parse(params.target).normalize()
                }
              await this._system.exit(
                makeEffects({
                  procedureId: params.id,
                }),
                target,
              )
            }
          })().then((result) => ({ result })),
        )
      })
      .when(initType, async ({ id, params }) => {
        return handleRpc(
          id,
          (async () => {
            if (!this._system) {
              const system = await this.getDependencies.system()
              this.callbacks = new CallbackHolder(
                makeEffects({
                  procedureId: params.id,
                }),
              )
              const callbacks = this.callbacks.child("init")
              console.error("Initializing...")
              await system.init(
                makeEffects({
                  procedureId: params.id,
                  callbacks,
                }),
                params.kind,
              )
              console.error("Initialization complete.")
              this._system = system
            }
          })().then((result) => ({ result })),
        )
      })
      .when(evalType, async ({ id, params }) => {
        return handleRpc(
          id,
          (async () => {
            const result = await new Function(
              `return (async () => { return (${params.script}) }).call(this)`,
            ).call({
              listener: this,
              require: require,
            })
            return {
              jsonrpc,
              id,
              result: ![
                "string",
                "number",
                "boolean",
                "null",
                "object",
              ].includes(typeof result)
                ? null
                : result,
            }
          })(),
        )
      })
      .when(
        shape({ id: idType.optional(), method: string }),
        ({ id, method }) => ({
          jsonrpc,
          id,
          error: {
            code: -32601,
            message: `Method not found`,
            data: {
              details: method,
            },
          },
        }),
      )

      .defaultToLazy(() => {
        console.warn(
          `Couldn't parse the following input ${JSON.stringify(input)}`,
        )
        return {
          jsonrpc,
          id: (input as any)?.id,
          error: {
            code: -32602,
            message: "invalid params",
            data: {
              details: JSON.stringify(input),
            },
          },
        }
      })
  }
  private getResult(
    procedure: typeof jsonPath._TYPE,
    system: System,
    procedureId: string,
    timeout: number | null | undefined,
    input: any,
  ) {
    const ensureResultTypeShape = (
      result: void | T.ActionInput | T.ActionResult | null,
    ): { result: any } => {
      return { result }
    }
    const callbacks = this.callbacks?.child(procedure)
    const effects = makeEffects({
      procedureId,
      callbacks,
    })

    return (async () => {
      switch (procedure) {
        case "/backup/create":
          return system.createBackup(effects, timeout || null)
        default:
          const procedures = unNestPath(procedure)
          switch (true) {
            case procedures[1] === "actions" && procedures[3] === "getInput":
              return system.getActionInput(
                effects,
                procedures[2],
                timeout || null,
              )
            case procedures[1] === "actions" && procedures[3] === "run":
              return system.runAction(
                effects,
                procedures[2],
                input.input,
                timeout || null,
              )
          }
      }
    })().then(ensureResultTypeShape, (error) =>
      matches(error)
        .when(
          object({
            error: string,
            code: number.defaultTo(0),
          }),
          (error) => ({
            error: {
              code: error.code,
              message: error.error,
            },
          }),
        )
        .defaultToLazy(() => ({
          error: {
            code: 0,
            message: String(error),
          },
        })),
    )
  }
}
