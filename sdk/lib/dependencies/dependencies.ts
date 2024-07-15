import {
  Effects,
  PackageId,
  DependencyRequirement,
  SetHealth,
  CheckDependenciesResult,
} from "../types"

export type CheckAllDependencies = {
  notInstalled: () => Promise<CheckDependenciesResult[]>
  notRunning: () => Promise<CheckDependenciesResult[]>
  configNotSatisfied: () => Promise<CheckDependenciesResult[]>
  healthErrors: () => Promise<{ [id: string]: SetHealth[] }>

  isValid: () => Promise<boolean>

  throwIfNotRunning: () => Promise<void>
  throwIfNotInstalled: () => Promise<void>
  throwIfConfigNotSatisfied: () => Promise<void>
  throwIfHealthError: () => Promise<void>

  throwIfNotValid: () => Promise<void>
}
export function checkAllDependencies(effects: Effects): CheckAllDependencies {
  const dependenciesPromise = effects.getDependencies()
  const resultsPromise = dependenciesPromise.then((dependencies) =>
    effects.checkDependencies({
      packageIds: dependencies.map((dep) => dep.id),
    }),
  )

  const dependenciesByIdPromise = dependenciesPromise.then((d) =>
    d.reduce(
      (acc, dep) => {
        acc[dep.id] = dep
        return acc
      },
      {} as { [id: PackageId]: DependencyRequirement },
    ),
  )

  const healthErrors = async () => {
    const results = await resultsPromise
    const dependenciesById = await dependenciesByIdPromise
    const answer: { [id: PackageId]: SetHealth[] } = {}
    for (const result of results) {
      const dependency = dependenciesById[result.packageId]
      if (!dependency) continue
      if (dependency.kind !== "running") continue

      const healthChecks = Object.entries(result.healthChecks)
        .map(([id, hc]) => ({ ...hc, id }))
        .filter((x) => !!x.message)
      if (healthChecks.length === 0) continue
      answer[result.packageId] = healthChecks
    }
    return answer
  }
  const configNotSatisfied = () =>
    resultsPromise.then((x) => x.filter((x) => !x.configSatisfied))
  const notInstalled = () =>
    resultsPromise.then((x) => x.filter((x) => !x.isInstalled))
  const notRunning = async () => {
    const results = await resultsPromise
    const dependenciesById = await dependenciesByIdPromise
    return results.filter((x) => {
      const dependency = dependenciesById[x.packageId]
      if (!dependency) return false
      if (dependency.kind !== "running") return false
      return !x.isRunning
    })
  }
  const entries = <B>(x: { [k: string]: B }) => Object.entries(x)
  const first = <A>(x: A[]): A | undefined => x[0]
  const sinkVoid = <A>(x: A) => void 0
  const throwIfHealthError = () =>
    healthErrors()
      .then(entries)
      .then(first)
      .then((x) => {
        if (!x) return
        const [id, healthChecks] = x
        if (healthChecks.length > 0)
          throw `Package ${id} has the following errors: ${healthChecks.map((x) => x.message).join(", ")}`
      })

  const throwIfConfigNotSatisfied = () =>
    configNotSatisfied().then((results) => {
      throw new Error(
        `Package ${results[0].packageId} does not have a valid configuration`,
      )
    })

  const throwIfNotRunning = () =>
    notRunning().then((results) => {
      if (results[0])
        throw new Error(`Package ${results[0].packageId} is not running`)
    })

  const throwIfNotInstalled = () =>
    notInstalled().then((results) => {
      if (results[0])
        throw new Error(`Package ${results[0].packageId} is not installed`)
    })
  const throwIfNotValid = async () =>
    Promise.all([
      throwIfNotRunning(),
      throwIfNotInstalled(),
      throwIfConfigNotSatisfied(),
      throwIfHealthError(),
    ]).then(sinkVoid)

  const isValid = () =>
    throwIfNotValid().then(
      () => true,
      () => false,
    )

  return {
    notRunning,
    notInstalled,
    configNotSatisfied,
    healthErrors,
    throwIfNotRunning,
    throwIfConfigNotSatisfied,
    throwIfNotValid,
    throwIfNotInstalled,
    throwIfHealthError,
    isValid,
  }
}
