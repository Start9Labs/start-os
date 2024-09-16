import { object, string } from "ts-matches"
import { Effects } from "../Effects"
import { Origin } from "./Origin"
import { AddSslOptions, BindParams } from "../osBindings"
import { Security } from "../osBindings"
import { BindOptions } from "../osBindings"
import { AlpnInfo } from "../osBindings"

export { AddSslOptions, Security, BindOptions }

export const knownProtocols = {
  http: {
    secure: null,
    defaultPort: 80,
    withSsl: "https",
    alpn: { specified: ["http/1.1"] } as AlpnInfo,
  },
  https: {
    secure: { ssl: true },
    defaultPort: 443,
  },
  ws: {
    secure: null,
    defaultPort: 80,
    withSsl: "wss",
    alpn: { specified: ["http/1.1"] } as AlpnInfo,
  },
  wss: {
    secure: { ssl: true },
    defaultPort: 443,
  },
  ssh: {
    secure: { ssl: false },
    defaultPort: 22,
  },
  bitcoin: {
    secure: { ssl: false },
    defaultPort: 8333,
  },
  lightning: {
    secure: { ssl: true },
    defaultPort: 9735,
  },
  grpc: {
    secure: { ssl: true },
    defaultPort: 50051,
  },
  dns: {
    secure: { ssl: false },
    defaultPort: 53,
  },
} as const

export type Scheme = string | null

type KnownProtocols = typeof knownProtocols
type ProtocolsWithSslVariants = {
  [K in keyof KnownProtocols]: KnownProtocols[K] extends {
    withSsl: string
  }
    ? K
    : never
}[keyof KnownProtocols]
type NotProtocolsWithSslVariants = Exclude<
  keyof KnownProtocols,
  ProtocolsWithSslVariants
>

type BindOptionsByKnownProtocol =
  | {
      protocol: ProtocolsWithSslVariants
      preferredExternalPort?: number
      addSsl?: Partial<AddSslOptions>
    }
  | {
      protocol: NotProtocolsWithSslVariants
      preferredExternalPort?: number
      addSsl?: AddSslOptions
    }
export type BindOptionsByProtocol = BindOptionsByKnownProtocol | BindOptions

export type HostKind = BindParams["kind"]

const hasStringProtocol = object({
  protocol: string,
}).test

export class Host {
  constructor(
    readonly options: {
      effects: Effects
      kind: HostKind
      id: string
    },
  ) {}

  /**
   * @description Use this function to bind the host to an internal port and inputSpecure options for protocol, security, and external port.
   *
   * @param internalPort - The internal port to be bound.
   * @param options - The protocol options for this binding.
   * @returns A multi-origin that is capable of exporting one or more service interfaces.
   * @example
   * In this example, we bind a previously created multi-host to port 80, then select the http protocol and request an external port of 8332.
   *
   * ```
    const uiMultiOrigin = await uiMulti.bindPort(80, {
      protocol: 'http',
      preferredExternalPort: 8332,
    })
   * ```
   */
  async bindPort(
    internalPort: number,
    options: BindOptionsByProtocol,
  ): Promise<Origin<this>> {
    if (hasStringProtocol(options)) {
      return await this.bindPortForKnown(options, internalPort)
    } else {
      return await this.bindPortForUnknown(internalPort, options)
    }
  }

  private async bindPortForUnknown(
    internalPort: number,
    options: {
      preferredExternalPort: number
      addSsl: AddSslOptions | null
      secure: { ssl: boolean } | null
    },
  ) {
    const binderOptions = {
      kind: this.options.kind,
      id: this.options.id,
      internalPort,
      ...options,
    }
    await this.options.effects.bind(binderOptions)

    return new Origin(this, internalPort, null, null)
  }

  private async bindPortForKnown(
    options: BindOptionsByKnownProtocol,
    internalPort: number,
  ) {
    const protoInfo = knownProtocols[options.protocol]
    const preferredExternalPort =
      options.preferredExternalPort ||
      knownProtocols[options.protocol].defaultPort
    const sslProto = this.getSslProto(options, protoInfo)
    const addSsl =
      sslProto && "alpn" in protoInfo
        ? {
            // addXForwardedHeaders: null,
            preferredExternalPort: knownProtocols[sslProto].defaultPort,
            scheme: sslProto,
            alpn: protoInfo.alpn,
            ...("addSsl" in options ? options.addSsl : null),
          }
        : null

    const secure: Security | null = !protoInfo.secure ? null : { ssl: false }

    await this.options.effects.bind({
      kind: this.options.kind,
      id: this.options.id,
      internalPort,
      preferredExternalPort,
      addSsl,
      secure,
    })

    return new Origin(this, internalPort, options.protocol, sslProto)
  }

  private getSslProto(
    options: BindOptionsByKnownProtocol,
    protoInfo: KnownProtocols[keyof KnownProtocols],
  ) {
    if (inObject("noAddSsl", options) && options.noAddSsl) return null
    if ("withSsl" in protoInfo && protoInfo.withSsl) return protoInfo.withSsl
    return null
  }
}

function inObject<Key extends string>(
  key: Key,
  obj: any,
): obj is { [K in Key]: unknown } {
  return key in obj
}

// export class StaticHost extends Host {
//   constructor(options: { effects: Effects; id: string }) {
//     super({ ...options, kind: "static" })
//   }
// }

// export class SingleHost extends Host {
//   constructor(options: { effects: Effects; id: string }) {
//     super({ ...options, kind: "single" })
//   }
// }

export class MultiHost extends Host {
  constructor(options: { effects: Effects; id: string }) {
    super({ ...options, kind: "multi" })
  }
}
