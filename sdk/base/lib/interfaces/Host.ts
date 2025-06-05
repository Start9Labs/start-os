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
export type BindOptionsByProtocol =
  | BindOptionsByKnownProtocol
  | (BindOptions & { protocol: null })

const hasStringProtocol = object({
  protocol: string,
}).test

export class MultiHost {
  constructor(
    readonly options: {
      effects: Effects
      id: string
    },
  ) {}

  /**
   * @description Use this function to bind the host to an internal port and configured options for protocol, security, and external port.
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
  ): Promise<Origin> {
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
    const sslProto = this.getSslProto(options)
    const addSsl = sslProto
      ? {
          // addXForwardedHeaders: null,
          preferredExternalPort: knownProtocols[sslProto].defaultPort,
          scheme: sslProto,
          alpn: "alpn" in protoInfo ? protoInfo.alpn : null,
          ...("addSsl" in options ? options.addSsl : null),
        }
      : options.addSsl
        ? {
            // addXForwardedHeaders: null,
            preferredExternalPort: 443,
            scheme: sslProto,
            alpn: null,
            ...("addSsl" in options ? options.addSsl : null),
          }
        : null

    const secure: Security | null = protoInfo.secure ?? null

    await this.options.effects.bind({
      id: this.options.id,
      internalPort,
      preferredExternalPort,
      addSsl,
      secure,
    })

    return new Origin(this, internalPort, options.protocol, sslProto)
  }

  private getSslProto(options: BindOptionsByKnownProtocol) {
    const proto = options.protocol
    const protoInfo = knownProtocols[proto]
    if (inObject("noAddSsl", options) && options.noAddSsl) return null
    if ("withSsl" in protoInfo && protoInfo.withSsl) return protoInfo.withSsl
    if (protoInfo.secure?.ssl) return proto
    return null
  }
}

function inObject<Key extends string>(
  key: Key,
  obj: any,
): obj is { [K in Key]: unknown } {
  return key in obj
}
