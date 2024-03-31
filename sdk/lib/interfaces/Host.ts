import { object, string } from "ts-matches"
import { Effects } from "../types"
import { Origin } from "./Origin"
import { AddSslOptions } from "../../../core/startos/bindings/AddSslOptions"
import { Security } from "../../../core/startos/bindings/Security"
import { BindOptions } from "../../../core/startos/bindings/BindOptions"
import { AlpnInfo } from "../../../core/startos/bindings/AlpnInfo"

export { AddSslOptions, Security, BindOptions }

const knownProtocols = {
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
      scheme?: Scheme
      addSsl?: Partial<AddSslOptions>
    }
  | {
      protocol: NotProtocolsWithSslVariants
      preferredExternalPort?: number
      scheme?: Scheme
      addSsl?: AddSslOptions
    }
type BindOptionsByProtocol = BindOptionsByKnownProtocol | BindOptions

export type HostKind = "static" | "single" | "multi"

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
      scheme: Scheme
      preferredExternalPort: number
      addSsl: AddSslOptions | null
      secure: { ssl: boolean } | null
    },
  ) {
    await this.options.effects.bind({
      kind: this.options.kind,
      id: this.options.id,
      internalPort: internalPort,
      ...options,
    })

    return new Origin(this, options)
  }

  private async bindPortForKnown(
    options: BindOptionsByKnownProtocol,
    internalPort: number,
  ) {
    const scheme =
      options.scheme === undefined ? options.protocol : options.scheme
    const protoInfo = knownProtocols[options.protocol]
    const preferredExternalPort =
      options.preferredExternalPort ||
      knownProtocols[options.protocol].defaultPort
    const addSsl = this.getAddSsl(options, protoInfo)

    const secure: Security | null = !protoInfo.secure ? null : { ssl: false }

    const newOptions = {
      scheme,
      preferredExternalPort,
      addSsl,
      secure,
    }

    await this.options.effects.bind({
      kind: this.options.kind,
      id: this.options.id,
      internalPort,
      ...newOptions,
    })

    return new Origin(this, newOptions)
  }

  private getAddSsl(
    options: BindOptionsByKnownProtocol,
    protoInfo: KnownProtocols[keyof KnownProtocols],
  ): AddSslOptions | null {
    if ("noAddSsl" in options && options.noAddSsl) return null
    if ("withSsl" in protoInfo && protoInfo.withSsl)
      return {
        // addXForwardedHeaders: null,
        preferredExternalPort: knownProtocols[protoInfo.withSsl].defaultPort,
        scheme: protoInfo.withSsl,
        alpn: protoInfo.alpn,
        ...("addSsl" in options ? options.addSsl : null),
      }
    return null
  }
}

export class StaticHost extends Host {
  constructor(options: { effects: Effects; id: string }) {
    super({ ...options, kind: "static" })
  }
}

export class SingleHost extends Host {
  constructor(options: { effects: Effects; id: string }) {
    super({ ...options, kind: "single" })
  }
}

export class MultiHost extends Host {
  constructor(options: { effects: Effects; id: string }) {
    super({ ...options, kind: "multi" })
  }
}
