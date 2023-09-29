import { Injectable } from '@angular/core'
import { Emver } from '@start9labs/shared'
import {
  filter,
  map,
  pairwise,
  shareReplay,
  startWith,
  tap,
} from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  HealthCheckResult,
  HealthResult,
  InstalledPackageDataEntry,
  PackageMainStatus,
} from './patch-db/data-model'
import * as deepEqual from 'fast-deep-equal'

export type AllDependencyErrors = Record<string, PkgDependencyErrors>
export type PkgDependencyErrors = Record<string, DependencyError | null>

@Injectable({
  providedIn: 'root',
})
export class DepErrorService {
  readonly depErrors$ = this.patch.watch$('package-data').pipe(
    map(pkgs =>
      Object.keys(pkgs)
        .map(id => ({
          id,
          depth: dependencyDepth(pkgs, id),
        }))
        .sort((a, b) => (b.depth > a.depth ? -1 : 1))
        .reduce(
          (errors, { id }): AllDependencyErrors => ({
            ...errors,
            [id]: this.getDepErrors(pkgs, id, errors),
          }),
          {} as AllDependencyErrors,
        ),
    ),
    startWith({} as AllDependencyErrors),
    pairwise(),
    filter(([prev, curr]) => !deepEqual(prev, curr)),
    map(([_, curr]) => curr),
    shareReplay(1),
  )

  constructor(
    private readonly emver: Emver,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  getPkgDepErrors$(pkgId: string) {
    return this.depErrors$.pipe(
      map(depErrors => depErrors[pkgId]),
      startWith({} as PkgDependencyErrors),
      pairwise(),
      filter(([prev, curr]) => !deepEqual(prev, curr)),
      map(([_, curr]) => curr),
      startWith({} as PkgDependencyErrors),
    )
  }

  private getDepErrors(
    pkgs: DataModel['package-data'],
    pkgId: string,
    outerErrors: AllDependencyErrors,
  ): PkgDependencyErrors {
    const pkgInstalled = pkgs[pkgId].installed

    if (!pkgInstalled) return {}

    return currentDeps(pkgs, pkgId).reduce(
      (innerErrors, depId): PkgDependencyErrors => ({
        ...innerErrors,
        [depId]: this.getDepError(pkgs, pkgInstalled, depId, outerErrors),
      }),
      {} as PkgDependencyErrors,
    )
  }

  private getDepError(
    pkgs: DataModel['package-data'],
    pkgInstalled: InstalledPackageDataEntry,
    depId: string,
    outerErrors: AllDependencyErrors,
  ): DependencyError | null {
    const depInstalled = pkgs[depId]?.installed

    // not installed
    if (!depInstalled) {
      return {
        type: DependencyErrorType.NotInstalled,
      }
    }

    const pkgManifest = pkgInstalled.manifest
    const depManifest = depInstalled.manifest

    // incorrect version
    if (
      !this.emver.satisfies(
        depManifest.version,
        pkgManifest.dependencies[depId].version,
      )
    ) {
      return {
        type: DependencyErrorType.IncorrectVersion,
        expected: pkgManifest.dependencies[depId].version,
        received: depManifest.version,
      }
    }

    // invalid config
    if (
      Object.values(pkgInstalled.status['dependency-config-errors']).some(
        err => !!err,
      )
    ) {
      return {
        type: DependencyErrorType.ConfigUnsatisfied,
      }
    }

    const depStatus = depInstalled.status.main.status

    // not running
    if (
      depStatus !== PackageMainStatus.Running &&
      depStatus !== PackageMainStatus.Starting &&
      !(
        depStatus === PackageMainStatus.BackingUp &&
        depInstalled.status.main.started
      )
    ) {
      return {
        type: DependencyErrorType.NotRunning,
      }
    }

    // health check failure
    if (depStatus === PackageMainStatus.Running) {
      for (let id of pkgInstalled['current-dependencies'][depId][
        'health-checks'
      ]) {
        if (
          depInstalled.status.main.health[id].result !== HealthResult.Success
        ) {
          return {
            type: DependencyErrorType.HealthChecksFailed,
            check: depInstalled.status.main.health[id],
          }
        }
      }
    }

    // transitive
    const transitiveError = currentDeps(pkgs, depId).some(transitiveId =>
      Object.values(outerErrors[transitiveId]).some(err => !!err),
    )

    if (transitiveError) {
      return {
        type: DependencyErrorType.Transitive,
      }
    }

    return null
  }
}

function currentDeps(pkgs: DataModel['package-data'], id: string): string[] {
  return Object.keys(
    pkgs[id]?.installed?.['current-dependencies'] || {},
  ).filter(depId => depId !== id)
}

function dependencyDepth(
  pkgs: DataModel['package-data'],
  id: string,
  depth = 0,
): number {
  return currentDeps(pkgs, id).reduce(
    (prev, depId) => dependencyDepth(pkgs, depId, prev + 1),
    depth,
  )
}

export type DependencyError =
  | DependencyErrorNotInstalled
  | DependencyErrorNotRunning
  | DependencyErrorIncorrectVersion
  | DependencyErrorConfigUnsatisfied
  | DependencyErrorHealthChecksFailed
  | DependencyErrorTransitive

export enum DependencyErrorType {
  NotInstalled = 'notInstalled',
  NotRunning = 'notRunning',
  IncorrectVersion = 'incorrectVersion',
  ConfigUnsatisfied = 'configUnsatisfied',
  HealthChecksFailed = 'healthChecksFailed',
  Transitive = 'transitive',
}

export interface DependencyErrorNotInstalled {
  type: DependencyErrorType.NotInstalled
}

export interface DependencyErrorNotRunning {
  type: DependencyErrorType.NotRunning
}

export interface DependencyErrorIncorrectVersion {
  type: DependencyErrorType.IncorrectVersion
  expected: string // version range
  received: string // version
}

export interface DependencyErrorConfigUnsatisfied {
  type: DependencyErrorType.ConfigUnsatisfied
}

export interface DependencyErrorHealthChecksFailed {
  type: DependencyErrorType.HealthChecksFailed
  check: HealthCheckResult
}

export interface DependencyErrorTransitive {
  type: DependencyErrorType.Transitive
}
