import { Injectable } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { map, shareReplay } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  HealthCheckResult,
  HealthResult,
  PackageDataEntry,
  PackageMainStatus,
} from './patch-db/data-model'

export type PackageDependencyErrors = Record<string, DependencyErrors>
export type DependencyErrors = Record<string, DependencyError | null>

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
          (errors, { id }): PackageDependencyErrors => ({
            ...errors,
            [id]: this.getDepErrors(pkgs, id, errors),
          }),
          {} as PackageDependencyErrors,
        ),
    ),
    shareReplay(1),
  )

  constructor(
    private readonly emver: Emver,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  private getDepErrors(
    pkgs: DataModel['package-data'],
    pkgId: string,
    outerErrors: PackageDependencyErrors,
  ): DependencyErrors {
    const pkg = pkgs[pkgId]

    if (!pkg.installed) return {}

    return currentDeps(pkgs, pkgId).reduce(
      (innerErrors, depId): DependencyErrors => ({
        ...innerErrors,
        [depId]: this.getDepError(pkgs, pkg, depId, outerErrors),
      }),
      {} as DependencyErrors,
    )
  }

  private getDepError(
    pkgs: DataModel['package-data'],
    pkg: PackageDataEntry,
    depId: string,
    outerErrors: PackageDependencyErrors,
  ): DependencyError | null {
    console.warn(depId)
    console.warn(pkgs)

    const dep = pkgs[depId]

    const pkgInstalled = pkg.installed!
    const depInstalled = dep?.installed

    // not installed
    if (!depInstalled) {
      return {
        type: DependencyErrorType.NotInstalled,
      }
    }

    const depStatus = depInstalled.status.main.status

    // backing up
    if (depStatus === PackageMainStatus.BackingUp) {
      return {
        type: DependencyErrorType.NotRunning,
      }
    }

    const pkgManifest = pkg.manifest
    const depManifest = dep.manifest

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

    // not running
    if (
      depStatus !== PackageMainStatus.Running &&
      depStatus !== PackageMainStatus.Starting
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
  BackingUp = 'backingUp',
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
