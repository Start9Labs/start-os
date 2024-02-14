import { object, string } from "ts-matches"
import { Effects } from "../types"
import { Origin } from "./Origin"

const knownProtocols = {
  http: {
    secure: false,
    ssl: false,
    defaultPort: 80,
    withSsl: "https",
  },
  https: {
    secure: true,
    ssl: true,
    defaultPort: 443,
  },
  ws: {
    secure: false,
    ssl: false,
    defaultPort: 80,
    withSsl: "wss",
  },
  wss: {
    secure: true,
    ssl: true,
    defaultPort: 443,
  },
  ssh: {
    secure: true,
    ssl: false,
    defaultPort: 22,
  },
  bitcoin: {
    secure: true,
    ssl: false,
    defaultPort: 8333,
  },
  grpc: {
    secure: true,
    ssl: true,
    defaultPort: 50051,
  },
  dns: {
    secure: true,
    ssl: false,
    defaultPort: 53,
  },
} as const

type Scheme = string | null

type AddSslOptions = {
  preferredExternalPort: number
  scheme: Scheme
  addXForwardedHeaders?: boolean /** default: false */
}
type Security = { secure: false; ssl: false } | { secure: true; ssl: boolean }
export type PortOptions = {
  scheme: Scheme
  preferredExternalPort: number
  addSsl: AddSslOptions | null
} & Security
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

type PortOptionsByKnownProtocol =
  | ({
      protocol: ProtocolsWithSslVariants
      preferredExternalPort?: number
      scheme?: Scheme
    } & ({ noAddSsl: true } | { addSsl?: Partial<AddSslOptions> }))
  | {
      protocol: NotProtocolsWithSslVariants
      preferredExternalPort?: number
      scheme?: Scheme
      addSsl?: AddSslOptions | null
    }
type PortOptionsByProtocol = PortOptionsByKnownProtocol | PortOptions

const hasStringProtocol = object({
  protocol: string,
}).test

export class Host {
  constructor(
    readonly options: {
      effects: Effects
      kind: "static" | "single" | "multi"
      id: string
    },
  ) {}

  async bindPort(
    internalPort: number,
    options: PortOptionsByProtocol,
  ): Promise<Origin<this>> {
    if (hasStringProtocol(options)) {
      return await this.bindPortForKnown(options, internalPort)
    } else {
      return await this.bindPortForUnknown(internalPort, options)
    }
  }

  private async bindPortForUnknown(
    internalPort: number,
    options:
      | ({
          scheme: Scheme
          preferredExternalPort: number
          addSsl: AddSslOptions | null
        } & { secure: false; ssl: false })
      | ({
          scheme: Scheme
          preferredExternalPort: number
          addSsl: AddSslOptions | null
        } & { secure: true; ssl: boolean }),
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
    options: PortOptionsByKnownProtocol,
    internalPort: number,
  ) {
    const scheme =
      options.scheme === undefined ? options.protocol : options.scheme
    const protoInfo = knownProtocols[options.protocol]
    const preferredExternalPort =
      options.preferredExternalPort ||
      knownProtocols[options.protocol].defaultPort
    const addSsl = this.getAddSsl(options, protoInfo)

    const security: Security = !protoInfo.secure
      ? {
          secure: protoInfo.secure,
          ssl: protoInfo.ssl,
        }
      : { secure: false, ssl: false }

    const newOptions = {
      scheme,
      preferredExternalPort,
      addSsl,
      ...security,
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
    options: PortOptionsByKnownProtocol,
    protoInfo: KnownProtocols[keyof KnownProtocols],
  ): AddSslOptions | null {
    if ("noAddSsl" in options && options.noAddSsl) return null
    if ("withSsl" in protoInfo && protoInfo.withSsl)
      return {
        preferredExternalPort: knownProtocols[protoInfo.withSsl].defaultPort,
        scheme: protoInfo.withSsl,
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
