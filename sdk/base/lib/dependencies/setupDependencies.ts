/**
 * @module setupDependencies
 *
 * This module provides utilities for declaring and managing service dependencies.
 * Dependencies allow services to declare that they require other services to be
 * installed and/or running before they can function properly.
 *
 * @example
 * ```typescript
 * // In dependencies.ts
 * export const setDependencies = sdk.setupDependencies(async ({ effects }) => {
 *   const config = await store.read(s => s.mediaSource).const(effects)
 *
 *   return {
 *     // Required dependency - must be running with passing health checks
 *     bitcoind: {
 *       kind: 'running',
 *       versionRange: '>=25.0.0',
 *       healthChecks: ['rpc']
 *     },
 *     // Optional dependency - only required if feature is enabled
 *     ...(config === 'nextcloud' ? {
 *       nextcloud: { kind: 'exists', versionRange: '>=28.0.0' }
 *     } : {})
 *   }
 * })
 * ```
 */

import * as T from "../types"
import { once } from "../util"

/**
 * Extracts the package IDs of required (non-optional) dependencies from a manifest.
 * Used for type-safe dependency declarations.
 *
 * @typeParam Manifest - The service manifest type
 */
export type RequiredDependenciesOf<Manifest extends T.SDKManifest> = {
  [K in keyof Manifest["dependencies"]]: Exclude<
    Manifest["dependencies"][K],
    undefined
  >["optional"] extends false
    ? K
    : never
}[keyof Manifest["dependencies"]]

/**
 * Extracts the package IDs of optional dependencies from a manifest.
 * These dependencies are declared in the manifest but marked as optional.
 *
 * @typeParam Manifest - The service manifest type
 */
export type OptionalDependenciesOf<Manifest extends T.SDKManifest> = Exclude<
  keyof Manifest["dependencies"],
  RequiredDependenciesOf<Manifest>
>

/**
 * Specifies the requirements for a single dependency.
 *
 * - `kind: "running"` - The dependency must be installed AND actively running
 *   with the specified health checks passing
 * - `kind: "exists"` - The dependency only needs to be installed (not necessarily running)
 */
type DependencyRequirement =
  | {
      /** The dependency must be running */
      kind: "running"
      /** Health check IDs that must be passing */
      healthChecks: Array<T.HealthCheckId>
      /** Semantic version range the dependency must satisfy (e.g., ">=1.0.0") */
      versionRange: string
    }
  | {
      /** The dependency only needs to be installed */
      kind: "exists"
      /** Semantic version range the dependency must satisfy */
      versionRange: string
    }

/** @internal Type checking helper */
type Matches<T, U> = T extends U ? (U extends T ? null : never) : never
const _checkType: Matches<
  DependencyRequirement & { id: T.PackageId },
  T.DependencyRequirement
> = null

/**
 * The return type for dependency declarations.
 * Required dependencies must always be specified; optional dependencies may be omitted.
 *
 * @typeParam Manifest - The service manifest type
 */
export type CurrentDependenciesResult<Manifest extends T.SDKManifest> = {
  [K in RequiredDependenciesOf<Manifest>]: DependencyRequirement
} & {
  [K in OptionalDependenciesOf<Manifest>]?: DependencyRequirement
}

/**
 * Creates a dependency setup function for use in the initialization pipeline.
 *
 * **Note:** This is exposed via `sdk.setupDependencies`. See the SDK documentation
 * for usage examples.
 *
 * The function you provide will be called during init to determine the current
 * dependency requirements, which may vary based on service configuration.
 *
 * @typeParam Manifest - The service manifest type (inferred from SDK)
 * @param fn - Async function that returns the current dependency requirements
 * @returns An init-compatible function that sets dependencies via Effects
 *
 * @see sdk.setupDependencies for usage documentation
 */
export function setupDependencies<Manifest extends T.SDKManifest>(
  fn: (options: {
    effects: T.Effects
  }) => Promise<CurrentDependenciesResult<Manifest>>,
): (effects: T.Effects) => Promise<null> {
  return async (effects: T.Effects) => {
    const dependencyType = await fn({ effects })
    return await effects.setDependencies({
      dependencies: Object.entries(dependencyType)
        .map(([k, v]) => [k, v as DependencyRequirement] as const)
        .map(
          ([id, { versionRange, ...x }]) =>
            ({
              id,
              ...x,
              versionRange: versionRange.toString(),
            }) as T.DependencyRequirement,
        ),
    })
  }
}
