/**
 * @module ServiceInterfaceBuilder
 *
 * Provides the ServiceInterfaceBuilder class for creating service interface
 * configurations that are exported to the StartOS UI.
 */

import { ServiceInterfaceType } from "../types"
import { Effects } from "../Effects"
import { Scheme } from "./Host"

/**
 * Builder class for creating service interface configurations.
 *
 * A service interface represents a user-visible endpoint in the StartOS UI.
 * It appears as a clickable link that users can use to access your service's
 * web UI, API, or other network resources.
 *
 * Interfaces are created with this builder and then exported via `Origin.export()`.
 *
 * @example
 * ```typescript
 * // Create a basic web UI interface
 * const webInterface = sdk.ServiceInterfaceBuilder.of({
 *   effects,
 *   name: 'Web Interface',
 *   id: 'webui',
 *   description: 'Access the main web dashboard',
 *   type: 'ui',
 * })
 *
 * // Create an API interface with a specific path
 * const apiInterface = sdk.ServiceInterfaceBuilder.of({
 *   effects,
 *   name: 'REST API',
 *   id: 'api',
 *   description: 'JSON API for programmatic access',
 *   type: 'api',
 *   path: '/api/v1',
 * })
 *
 * // Create an interface with basic auth
 * const protectedInterface = sdk.ServiceInterfaceBuilder.of({
 *   effects,
 *   name: 'Admin Panel',
 *   id: 'admin',
 *   description: 'Protected admin area',
 *   type: 'ui',
 *   username: 'admin',
 *   masked: true, // Hide the URL from casual viewing
 * })
 *
 * // Export all interfaces on the same origin
 * await origin.export([webInterface, apiInterface, protectedInterface])
 * ```
 */
export class ServiceInterfaceBuilder {
  constructor(
    readonly options: {
      /** Effects instance for system operations */
      effects: Effects
      /** Display name shown in the StartOS UI */
      name: string
      /** Unique identifier for this interface */
      id: string
      /** Description shown below the interface name */
      description: string
      /**
       * Type of interface:
       * - `"ui"` - Web interface (opens in browser)
       * - `"api"` - API endpoint (for programmatic access)
       * - `"p2p"` - Peer-to-peer endpoint (e.g., Bitcoin P2P)
       */
      type: ServiceInterfaceType
      /** Username for basic auth URLs (null for no auth) */
      username: string | null
      /** URL path to append (e.g., "/admin", "/api/v1") */
      path: string
      /** Query parameters to append to the URL */
      query: Record<string, string>
      /** Override default protocol schemes */
      schemeOverride: { ssl: Scheme; noSsl: Scheme } | null
      /** If true, the URL is hidden/masked in the UI (for sensitive endpoints) */
      masked: boolean
    },
  ) {}
}
