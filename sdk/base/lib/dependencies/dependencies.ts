import { ExtendedVersion, VersionRange } from "../exver"
import { PackageId, HealthCheckId } from "../types"
import { Effects } from "../Effects"

export type CheckDependencies<DependencyId extends PackageId = PackageId> = {
  installedSatisfied: (packageId: DependencyId) => boolean
  installedVersionSatisfied: (packageId: DependencyId) => boolean
  runningSatisfied: (packageId: DependencyId) => boolean
  actionsSatisfied: (packageId: DependencyId) => boolean
  healthCheckSatisfied: (
    packageId: DependencyId,
    healthCheckId: HealthCheckId,
  ) => boolean
  satisfied: () => boolean

  throwIfInstalledNotSatisfied: (packageId: DependencyId) => void
  throwIfInstalledVersionNotSatisfied: (packageId: DependencyId) => void
  throwIfRunningNotSatisfied: (packageId: DependencyId) => void
  throwIfActionsNotSatisfied: (packageId: DependencyId) => void
  throwIfHealthNotSatisfied: (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => void
  throwIfNotSatisfied: (packageId?: DependencyId) => void
}
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

  const find = (packageId: DependencyId) => {
    const dependencyRequirement = dependencies.find((d) => d.id === packageId)
    const dependencyResult = results.find((d) => d.packageId === packageId)
    if (!dependencyRequirement || !dependencyResult) {
      throw new Error(`Unknown DependencyId ${packageId}`)
    }
    return { requirement: dependencyRequirement, result: dependencyResult }
  }

  const installedSatisfied = (packageId: DependencyId) =>
    !!find(packageId).result.installedVersion
  const installedVersionSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
    return (
      !!dep.result.installedVersion &&
      ExtendedVersion.parse(dep.result.installedVersion).satisfies(
        VersionRange.parse(dep.requirement.versionRange),
      )
    )
  }
  const runningSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
    return dep.requirement.kind !== "running" || dep.result.isRunning
  }
  const actionsSatisfied = (packageId: DependencyId) =>
    Object.keys(find(packageId).result.requestedActions).length === 0
  const healthCheckSatisfied = (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => {
    const dep = find(packageId)
    if (
      healthCheckId &&
      (dep.requirement.kind !== "running" ||
        !dep.requirement.healthChecks.includes(healthCheckId))
    ) {
      throw new Error(`Unknown HealthCheckId ${healthCheckId}`)
    }
    const errors = Object.entries(dep.result.healthChecks)
      .filter(([id, _]) => (healthCheckId ? id === healthCheckId : true))
      .filter(([_, res]) => res.result !== "success")
    return errors.length === 0
  }
  const pkgSatisfied = (packageId: DependencyId) =>
    installedSatisfied(packageId) &&
    installedVersionSatisfied(packageId) &&
    runningSatisfied(packageId) &&
    actionsSatisfied(packageId) &&
    healthCheckSatisfied(packageId)
  const satisfied = (packageId?: DependencyId) =>
    packageId
      ? pkgSatisfied(packageId)
      : dependencies.every((d) => pkgSatisfied(d.id as DependencyId))

  const throwIfInstalledNotSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
    if (!dep.result.installedVersion) {
      throw new Error(`${dep.result.title || packageId} is not installed`)
    }
  }
  const throwIfInstalledVersionNotSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
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
  }
  const throwIfRunningNotSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
    if (dep.requirement.kind === "running" && !dep.result.isRunning) {
      throw new Error(`${dep.result.title || packageId} is not running`)
    }
  }
  const throwIfActionsNotSatisfied = (packageId: DependencyId) => {
    const dep = find(packageId)
    const reqs = Object.keys(dep.result.requestedActions)
    if (reqs.length) {
      throw new Error(
        `The following action requests have not been fulfilled: ${reqs.join(", ")}`,
      )
    }
  }
  const throwIfHealthNotSatisfied = (
    packageId: DependencyId,
    healthCheckId?: HealthCheckId,
  ) => {
    const dep = find(packageId)
    if (
      healthCheckId &&
      (dep.requirement.kind !== "running" ||
        !dep.requirement.healthChecks.includes(healthCheckId))
    ) {
      throw new Error(`Unknown HealthCheckId ${healthCheckId}`)
    }
    const errors = Object.entries(dep.result.healthChecks)
      .filter(([id, _]) => (healthCheckId ? id === healthCheckId : true))
      .filter(([_, res]) => res.result !== "success")
    if (errors.length) {
      throw new Error(
        errors
          .map(
            ([_, e]) =>
              `Health Check ${e.name} of ${dep.result.title || packageId} failed with status ${e.result}${e.message ? `: ${e.message}` : ""}`,
          )
          .join("; "),
      )
    }
  }
  const throwIfPkgNotSatisfied = (packageId: DependencyId) => {
    throwIfInstalledNotSatisfied(packageId)
    throwIfInstalledVersionNotSatisfied(packageId)
    throwIfRunningNotSatisfied(packageId)
    throwIfActionsNotSatisfied(packageId)
    throwIfHealthNotSatisfied(packageId)
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
        })()

  return {
    installedSatisfied,
    installedVersionSatisfied,
    runningSatisfied,
    actionsSatisfied,
    healthCheckSatisfied,
    satisfied,
    throwIfInstalledNotSatisfied,
    throwIfInstalledVersionNotSatisfied,
    throwIfRunningNotSatisfied,
    throwIfActionsNotSatisfied,
    throwIfHealthNotSatisfied,
    throwIfNotSatisfied,
  }
}
