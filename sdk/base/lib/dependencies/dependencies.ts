/**
 * @module dependencies
 *
 * This module provides utilities for checking whether service dependencies are satisfied.
 * Use `checkDependencies()` to get a helper object with methods for querying and
 * validating dependency status.
 *
 * @example
 * ```typescript
 * const deps = await checkDependencies(effects, ['bitcoind', 'lnd'])
 *
 * // Check if all dependencies are satisfied
 * if (deps.satisfied()) {
 *   // All good, proceed
 * }
 *
 * // Or throw an error with details if not satisfied
 * deps.throwIfNotSatisfied()
 *
 * // Check specific aspects
 * if (deps.installedSatisfied('bitcoind') && deps.runningSatisfied('bitcoind')) {
 *   // bitcoind is installed and running
 * }
 * ```
 */

import { ExtendedVersion, VersionRange } from "../exver"
import {
  PackageId,
  HealthCheckId,
  DependencyRequirement,
  CheckDependenciesResult,
} from "../types"
import { Effects } from "../Effects"

/**
 * Interface providing methods to check and validate dependency satisfaction.
 * Returned by `checkDependencies()`.
 *
 * @typeParam DependencyId - The type of package IDs being checked
 */
export type CheckDependencies<DependencyId extends PackageId = PackageId> = {
  /**
   * Gets the requirement and current result for a specific dependency.
   * @param packageId - The package ID to query
   * @returns Object containing the requirement spec and check result
   * @throws Error if the packageId is not a known dependency
   */
  infoFor: (packageId: DependencyId) => {
    requirement: DependencyRequirement
    result: CheckDependenciesResult
  }

  /**
   * Checks if a dependency is installed (regardless of version).
   * @param packageId - The package ID to check
   * @returns True if the package is installed
   */
  installedSatisfied: (packageId: DependencyId) => boolean

  /**
   * Checks if a dependency is installed with a satisfying version.
   * @param packageId - The package ID to check
   * @returns True if the installed version satisfies the version range requirement
   */
  installedVersionSatisfied: (packageId: DependencyId) => boolean

  /**
   * Checks if a "running" dependency is actually running.
   * Always returns true for "exists" dependencies.
   * @param packageId - The package ID to check
   * @returns True if the dependency is running (or only needs to exist)
   */
  runningSatisfied: (packageId: DependencyId) => boolean

  /**
   * Checks if all critical tasks for a dependency have been completed.
   * @param packageId - The package ID to check
   * @returns True if no critical tasks are pending
   */
  tasksSatisfied: (packageId: DependencyId) => boolean

  /**
   * Checks if specified health checks are passing for a dependency.
   * @param packageId - The package ID to check
   * @param healthCheckId - Specific health check to verify (optional - checks all if omitted)
   * @returns True if the health check(s) are passing
   */
  healthCheckSatisfied: (
    packageId: DependencyId,
    healthCheckId: HealthCheckId,
  ) => boolean

  /**
   * Checks if all dependencies are fully satisfied.
   * @returns True if all dependencies meet all requirements
   */
  satisfied: () => boolean

  /**
   * Throws an error if the dependency is not installed.
   * @param packageId - The package ID to check
   * @throws Error with message if not installed
   */
  throwIfInstalledNotSatisfied: (packageId: DependencyId) => null

  /**
   * Throws an error if the installed version doesn't satisfy requirements.
   * @param packageId - The package ID to check
   * @throws Error with version mismatch details if not satisfied
   */
  throwIfInstalledVersionNotSatisfied: (packageId: DependencyId) => null

  /**
   * Throws an error if a "running" dependency is not running.
   * @param packageId - The package ID to check
   * @throws Error if the dependency should be running but isn't
   */
  throwIfRunningNotSatisfied: (packageId: DependencyId) => null

  /**
   * Throws an error if critical tasks are pending for the dependency.
   * @param packageId - The package ID to check
   * @throws Error listing pending critical tasks
   */
  throwIfTasksNotSatisfied: (packageId: DependencyId) => null

  /**
   * Throws an error if health checks are failing for the dependency.
   * @param packageId - The package ID to check
   * @param healthCheckId - Specific health check (optional - checks all if omitted)
   * @throws Error with health check failure details
   */
  throwIfHealthNotSatisfied: (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => null

  /**
   * Throws an error if any requirements are not satisfied.
   * @param packageId - Specific package to check (optional - checks all if omitted)
   * @throws Error with detailed message about what's not satisfied
   */
  throwIfNotSatisfied: (packageId?: DependencyId) => null
}
/**
 * Checks the satisfaction status of service dependencies.
 * Returns a helper object with methods to query and validate dependency status.
 *
 * This is useful for:
 * - Verifying dependencies before starting operations that require them
 * - Providing detailed error messages about unsatisfied dependencies
 * - Conditionally enabling features based on dependency availability
 *
 * @typeParam DependencyId - The type of package IDs (defaults to string)
 *
 * @param effects - Effects instance for system operations
 * @param packageIds - Optional array of specific dependencies to check (checks all if omitted)
 * @returns Promise resolving to a CheckDependencies helper object
 *
 * @example
 * ```typescript
 * // Check all dependencies
 * const deps = await checkDependencies(effects)
 * deps.throwIfNotSatisfied() // Throws if any dependency isn't met
 *
 * // Check specific dependencies
 * const deps = await checkDependencies(effects, ['bitcoind'])
 * if (deps.runningSatisfied('bitcoind') && deps.healthCheckSatisfied('bitcoind', 'rpc')) {
 *   // Safe to make RPC calls to bitcoind
 * }
 *
 * // Get detailed info
 * const info = deps.infoFor('bitcoind')
 * console.log(`Installed: ${info.result.installedVersion}`)
 * console.log(`Running: ${info.result.isRunning}`)
 * ```
 */
export async function checkDependencies<
  DependencyId extends PackageId = PackageId,
>(
  effects: Effects,
  packageIds?: DependencyId[],
): Promise<CheckDependencies<DependencyId>> {
  let [dependencies, results] = await Promise.all([
    effects.getDependencies(),
    effects.checkDependencies({
      packageIds,
    }),
  ])
  if (packageIds) {
    dependencies = dependencies.filter((d) =>
      (packageIds as PackageId[]).includes(d.id),
    )
  }

  const infoFor = (packageId: DependencyId) => {
    const dependencyRequirement = dependencies.find((d) => d.id === packageId)
    const dependencyResult = results.find((d) => d.packageId === packageId)
    if (!dependencyRequirement || !dependencyResult) {
      throw new Error(`Unknown DependencyId ${packageId}`)
    }
    return { requirement: dependencyRequirement, result: dependencyResult }
  }

  const installedSatisfied = (packageId: DependencyId) =>
    !!infoFor(packageId).result.installedVersion
  const installedVersionSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    return (
      !!dep.result.installedVersion &&
      ExtendedVersion.parse(dep.result.installedVersion).satisfies(
        VersionRange.parse(dep.requirement.versionRange),
      )
    )
  }
  const runningSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    return dep.requirement.kind !== "running" || dep.result.isRunning
  }
  const tasksSatisfied = (packageId: DependencyId) =>
    Object.entries(infoFor(packageId).result.tasks).filter(
      ([_, t]) => t?.active && t.task.severity === "critical",
    ).length === 0
  const healthCheckSatisfied = (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => {
    const dep = infoFor(packageId)
    if (
      healthCheckId &&
      (dep.requirement.kind !== "running" ||
        !dep.requirement.healthChecks.includes(healthCheckId))
    ) {
      throw new Error(`Unknown HealthCheckId ${healthCheckId}`)
    }
    const errors =
      dep.requirement.kind === "running"
        ? dep.requirement.healthChecks
            .map((id) => [id, dep.result.healthChecks[id] ?? null] as const)
            .filter(([id, _]) => (healthCheckId ? id === healthCheckId : true))
            .filter(([_, res]) => res?.result !== "success")
        : []
    return errors.length === 0
  }
  const pkgSatisfied = (packageId: DependencyId) =>
    installedSatisfied(packageId) &&
    installedVersionSatisfied(packageId) &&
    runningSatisfied(packageId) &&
    tasksSatisfied(packageId) &&
    healthCheckSatisfied(packageId)
  const satisfied = (packageId?: DependencyId) =>
    packageId
      ? pkgSatisfied(packageId)
      : dependencies.every((d) => pkgSatisfied(d.id as DependencyId))

  const throwIfInstalledNotSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    if (!dep.result.installedVersion) {
      throw new Error(`${dep.result.title || packageId} is not installed`)
    }
    return null
  }
  const throwIfInstalledVersionNotSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    if (!dep.result.installedVersion) {
      throw new Error(`${dep.result.title || packageId} is not installed`)
    }
    if (
      ![dep.result.installedVersion, ...dep.result.satisfies].find((v) =>
        ExtendedVersion.parse(v).satisfies(
          VersionRange.parse(dep.requirement.versionRange),
        ),
      )
    ) {
      throw new Error(
        `Installed version ${dep.result.installedVersion} of ${dep.result.title || packageId} does not match expected version range ${dep.requirement.versionRange}`,
      )
    }
    return null
  }
  const throwIfRunningNotSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    if (dep.requirement.kind === "running" && !dep.result.isRunning) {
      throw new Error(`${dep.result.title || packageId} is not running`)
    }
    return null
  }
  const throwIfTasksNotSatisfied = (packageId: DependencyId) => {
    const dep = infoFor(packageId)
    const reqs = Object.entries(dep.result.tasks)
      .filter(([_, t]) => t?.active && t.task.severity === "critical")
      .map(([id, _]) => id)
    if (reqs.length) {
      throw new Error(
        `The following action requests have not been fulfilled: ${reqs.join(", ")}`,
      )
    }
    return null
  }
  const throwIfHealthNotSatisfied = (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => {
    const dep = infoFor(packageId)
    if (
      healthCheckId &&
      (dep.requirement.kind !== "running" ||
        !dep.requirement.healthChecks.includes(healthCheckId))
    ) {
      throw new Error(`Unknown HealthCheckId ${healthCheckId}`)
    }
    const errors =
      dep.requirement.kind === "running"
        ? dep.requirement.healthChecks
            .map((id) => [id, dep.result.healthChecks[id] ?? null] as const)
            .filter(([id, _]) => (healthCheckId ? id === healthCheckId : true))
            .filter(([_, res]) => res?.result !== "success")
        : []
    if (errors.length) {
      throw new Error(
        errors
          .map(([id, e]) =>
            e
              ? `Health Check ${e.name} of ${dep.result.title || packageId} failed with status ${e.result}${e.message ? `: ${e.message}` : ""}`
              : `Health Check ${id} of ${dep.result.title} does not exist`,
          )
          .join("; "),
      )
    }
    return null
  }
  const throwIfPkgNotSatisfied = (packageId: DependencyId) => {
    throwIfInstalledNotSatisfied(packageId)
    throwIfInstalledVersionNotSatisfied(packageId)
    throwIfRunningNotSatisfied(packageId)
    throwIfTasksNotSatisfied(packageId)
    throwIfHealthNotSatisfied(packageId)
    return null
  }
  const throwIfNotSatisfied = (packageId?: DependencyId) =>
    packageId
      ? throwIfPkgNotSatisfied(packageId)
      : (() => {
          const err = dependencies.flatMap((d) => {
            try {
              throwIfPkgNotSatisfied(d.id as DependencyId)
            } catch (e) {
              if (e instanceof Error) return [e.message]
              throw e
            }
            return []
          })
          if (err.length) {
            throw new Error(err.join("; "))
          }
          return null
        })()

  return {
    infoFor,
    installedSatisfied,
    installedVersionSatisfied,
    runningSatisfied,
    tasksSatisfied,
    healthCheckSatisfied,
    satisfied,
    throwIfInstalledNotSatisfied,
    throwIfInstalledVersionNotSatisfied,
    throwIfRunningNotSatisfied,
    throwIfTasksNotSatisfied,
    throwIfHealthNotSatisfied,
    throwIfNotSatisfied,
  }
}
