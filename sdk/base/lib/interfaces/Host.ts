/**
 * @module Host
 *
 * This module provides the MultiHost class for binding network ports and
 * exposing service interfaces through the StartOS networking layer.
 *
 * MultiHost handles the complexity of:
 * - Port binding with protocol-specific defaults
 * - Automatic SSL/TLS setup for secure protocols
 * - Integration with Tor and LAN networking
 */

import { object, string } from "ts-matches"
import { Effects } from "../Effects"
import { Origin } from "./Origin"
import { AddSslOptions, BindParams } from "../osBindings"
import { Security } from "../osBindings"
import { BindOptions } from "../osBindings"
import { AlpnInfo } from "../osBindings"

export { AddSslOptions, Security, BindOptions }

/**
 * Known protocol definitions with their default ports and SSL variants.
 *
 * Each protocol includes:
 * - `secure` - Whether the protocol is inherently secure (SSL/TLS)
 * - `defaultPort` - Standard port for this protocol
 * - `withSsl` - The SSL variant of the protocol (if applicable)
 * - `alpn` - ALPN negotiation info for TLS
 */
export const knownProtocols = {
  /** HTTP - plain text web traffic, auto-upgrades to HTTPS */
  http: {
    secure: null,
    defaultPort: 80,
    withSsl: "https",
    alpn: { specified: ["http/1.1"] } as AlpnInfo,
  },
  /** HTTPS - encrypted web traffic */
  https: {
    secure: { ssl: true },
    defaultPort: 443,
  },
  /** WebSocket - plain text, auto-upgrades to WSS */
  ws: {
    secure: null,
    defaultPort: 80,
    withSsl: "wss",
    alpn: { specified: ["http/1.1"] } as AlpnInfo,
  },
  /** Secure WebSocket */
  wss: {
    secure: { ssl: true },
    defaultPort: 443,
  },
  /** SSH - inherently secure (no SSL wrapper needed) */
  ssh: {
    secure: { ssl: false },
    defaultPort: 22,
  },
  /** DNS - domain name service */
  dns: {
    secure: { ssl: false },
    defaultPort: 53,
  },
} as const

/** Protocol scheme string or null for no scheme */
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

/** @internal Helper to detect if protocol is a known protocol string */
const hasStringProtocol = object({
  protocol: string,
}).test

/**
 * Manages network bindings for a service interface.
 *
 * MultiHost is the primary way to expose your service's ports to users.
 * It handles:
 * - Port binding with the StartOS networking layer
 * - Protocol-aware defaults (HTTP uses port 80, HTTPS uses 443, etc.)
 * - Automatic SSL certificate provisioning for secure protocols
 * - Creation of Origin objects for exporting service interfaces
 *
 * @example
 * ```typescript
 * // Create a host for the web UI
 * const webHost = sdk.MultiHost.of(effects, 'webui')
 *
 * // Bind port 3000 with HTTP (automatically adds HTTPS variant)
 * const webOrigin = await webHost.bindPort(3000, { protocol: 'http' })
 *
 * // Export the interface
 * await webOrigin.export([
 *   sdk.ServiceInterfaceBuilder.of({
 *     effects,
 *     name: 'Web Interface',
 *     id: 'webui',
 *     description: 'Access the dashboard',
 *     type: 'ui',
 *   })
 * ])
 * ```
 */
export class MultiHost {
  constructor(
    readonly options: {
      /** Effects instance for system operations */
      effects: Effects
      /** Unique identifier for this host binding */
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
          addXForwardedHeaders: false,
          preferredExternalPort: knownProtocols[sslProto].defaultPort,
          scheme: sslProto,
          alpn: "alpn" in protoInfo ? protoInfo.alpn : null,
          ...("addSsl" in options ? options.addSsl : null),
        }
      : options.addSsl
        ? {
            addXForwardedHeaders: false,
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
