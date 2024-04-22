import {
  Effects,
  PackageId,
  DependencyRequirement,
  SetHealth,
  CheckDependencyResult,
} from "../types"

export type CheckAllDependencies = {
  notRunning: () => Promise<CheckDependencyResult[]>

  notInstalled: () => Promise<CheckDependencyResult[]>

  errorMessages: () => Promise<{ [id: string]: SetHealth[] }>
  throwIfNotRunning: () => Promise<void>
  throwIfNotValid: () => Promise<undefined>
  throwIfNotInstalled: () => Promise<void>
  throwIfError: () => Promise<void>
  isValid: () => Promise<boolean>
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

  const errorMessages = async () => {
    const results = await resultsPromise
    const dependenciesById = await dependenciesByIdPromise
    const answer: { [id: PackageId]: SetHealth[] } = {}
    for (const result of results) {
      const dependency = dependenciesById[result.packageId]
      if (!dependency) continue
      if (dependency.kind !== "running") continue

      const healthChecks = result.healthChecks
        .filter((x) => dependency.healthChecks.includes(x.id))
        .filter((x) => !!x.message)
      if (healthChecks.length === 0) continue
      answer[result.packageId] = healthChecks
    }
    return answer
  }
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
  const throwIfError = () =>
    errorMessages()
      .then(entries)
      .then(first)
      .then((x) => {
        if (!x) return
        const [id, healthChecks] = x
        if (healthChecks.length > 0)
          throw `Package ${id} has the following errors: ${healthChecks.map((x) => x.message).join(", ")}`
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
      throwIfError(),
    ]).then(sinkVoid)

  const isValid = () =>
    throwIfNotValid().then(
      () => true,
      () => false,
    )

  return {
    notRunning,
    notInstalled,
    errorMessages,
    throwIfNotRunning,
    throwIfNotValid,
    throwIfNotInstalled,
    throwIfError,
    isValid,
  }
}
