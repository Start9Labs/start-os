import { ValidateExVerRange } from '../exver'
import * as T from '../types'
import { once } from '../util'

export type RequiredDependenciesOf<Manifest extends T.SDKManifest> = {
  [K in keyof Manifest['dependencies']]: Exclude<
    Manifest['dependencies'][K],
    undefined
  >['optional'] extends false
    ? K
    : never
}[keyof Manifest['dependencies']]
export type OptionalDependenciesOf<Manifest extends T.SDKManifest> = Exclude<
  keyof Manifest['dependencies'],
  RequiredDependenciesOf<Manifest>
>

type DependencyRequirement =
  | {
      kind: 'running'
      healthChecks: Array<T.HealthCheckId>
      versionRange: string
    }
  | {
      kind: 'exists'
      versionRange: string
    }
type Matches<T, U> = T extends U ? (U extends T ? null : never) : never
const _checkType: Matches<
  DependencyRequirement & { id: T.PackageId },
  T.DependencyRequirement
> = null

export type CurrentDependenciesResult<Manifest extends T.SDKManifest> = {
  [K in RequiredDependenciesOf<Manifest>]: DependencyRequirement
} & {
  [K in OptionalDependenciesOf<Manifest>]?: DependencyRequirement
}

/**
 * Validates each requirement's `versionRange` literal as an exver version
 * range. A malformed range collapses that field to `never`, surfacing a compile
 * error at the offending requirement. Non-literal `string` values pass through
 * unchecked (see {@link ValidateExVerRange}); runtime validation is unchanged.
 */
export type ValidateVersionRanges<R> = {
  [K in keyof R]: R[K] extends { versionRange: infer V }
    ? V extends string
      ? [ValidateExVerRange<V>] extends [never]
        ? Omit<R[K], 'versionRange'> & { versionRange: never }
        : R[K]
      : R[K]
    : R[K]
}

export function setupDependencies<
  Manifest extends T.SDKManifest,
  const R extends CurrentDependenciesResult<Manifest>,
>(
  fn: (options: {
    effects: T.Effects
  }) => Promise<R & ValidateVersionRanges<R>>,
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

function __type_tests() {
  type TestManifest = Omit<T.SDKManifest, 'dependencies'> & {
    dependencies: {
      'hello-world': T.Manifest['dependencies'][string] & { optional: false }
    }
  }
  // `Manifest` is fixed and `R` stays inferred — the same wrapper shape that
  // `sdk.setupDependencies` uses, so `const R` can capture range literals.
  const setup = <const R extends CurrentDependenciesResult<TestManifest>>(
    fn: (options: {
      effects: T.Effects
    }) => Promise<R & ValidateVersionRanges<R>>,
  ) => setupDependencies<TestManifest, R>(fn)

  // valid range literal (full grammar)
  setup(async () => ({
    'hello-world': {
      kind: 'running',
      versionRange: '>=1.0.0:0 && <2:0',
      healthChecks: [],
    },
  }))

  // invalid range literal — error lands on the returned object
  setup(async () =>
    // @ts-expect-error - malformed version in range
    ({
      'hello-world': {
        kind: 'running',
        versionRange: '>=2.f',
        healthChecks: [],
      },
    }),
  )
}
