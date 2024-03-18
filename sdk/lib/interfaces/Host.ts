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
  lightning: {
    secure: true,
    ssl: true,
    defaultPort: 9735,
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

export type Scheme = string | null

type AddSslOptions = {
  scheme: Scheme
  preferredExternalPort: number
  addXForwardedHeaders: boolean | null /** default: false */
}
type Security = { ssl: boolean }
export type BindOptions = {
  scheme: Scheme
  preferredExternalPort: number
  addSsl: AddSslOptions | null
  secure: Security | null
}
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
      preferredExternalPort: number | null
      scheme: Scheme | null
      addSsl: Partial<AddSslOptions> | null
    }
  | {
      protocol: NotProtocolsWithSslVariants
      preferredExternalPort: number | null
      scheme: Scheme | null
      addSsl: AddSslOptions | null
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
        addXForwardedHeaders: null,
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
