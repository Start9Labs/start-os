import { Injectable } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { distinctUntilChanged, map, shareReplay } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
} from './patch-db/data-model'
import * as deepEqual from 'fast-deep-equal'
import { isInstalled } from '../util/get-package-data'
import { DependencyError } from './api/api.types'

export type AllDependencyErrors = Record<string, PkgDependencyErrors>
export type PkgDependencyErrors = Record<string, DependencyError | null>

@Injectable({
  providedIn: 'root',
})
export class DepErrorService {
  readonly depErrors$ = this.patch.watch$('packageData').pipe(
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
    distinctUntilChanged(deepEqual),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  constructor(
    private readonly exver: Exver,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  getPkgDepErrors$(pkgId: string) {
    return this.depErrors$.pipe(
      map(depErrors => depErrors[pkgId]),
      distinctUntilChanged(deepEqual),
    )
  }

  private getDepErrors(
    pkgs: DataModel['packageData'],
    pkgId: string,
    outerErrors: AllDependencyErrors,
  ): PkgDependencyErrors {
    const pkg = pkgs[pkgId]

    if (!isInstalled(pkg)) return {}

    return currentDeps(pkgs, pkgId).reduce(
      (innerErrors, depId): PkgDependencyErrors => ({
        ...innerErrors,
        [depId]: this.getDepError(pkgs, pkg, depId, outerErrors),
      }),
      {} as PkgDependencyErrors,
    )
  }

  private getDepError(
    pkgs: DataModel['packageData'],
    pkg: PackageDataEntry<InstalledState>,
    depId: string,
    outerErrors: AllDependencyErrors,
  ): DependencyError | null {
    const dep = pkgs[depId]

    // not installed
    if (!dep || dep.stateInfo.state !== 'installed') {
      return {
        type: 'notInstalled',
      }
    }

    const currentDep = pkg.currentDependencies[depId]
    const depManifest = dep.stateInfo.manifest

    // incorrect version
    if (!this.exver.satisfies(depManifest.version, currentDep.versionRange)) {
      if (
        depManifest.satisfies.some(
          v => !this.exver.satisfies(v, currentDep.versionRange),
        )
      ) {
        return {
          type: 'incorrectVersion',
          expected: currentDep.versionRange,
          received: depManifest.version,
        }
      }
    }

    // invalid config
    if (!currentDep.configSatisfied) {
      return {
        type: 'configUnsatisfied',
      }
    }

    const depStatus = dep.status.main.status

    // not running
    if (depStatus !== 'running' && depStatus !== 'starting') {
      return {
        type: 'notRunning',
      }
    }

    // health check failure
    if (depStatus === 'running' && currentDep.kind === 'running') {
      for (let id of currentDep.healthChecks) {
        const check = dep.status.main.health[id]
        if (check?.result !== 'success') {
          return {
            type: 'healthChecksFailed',
            check,
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
        type: 'transitive',
      }
    }

    return null
  }
}

function currentDeps(pkgs: DataModel['packageData'], id: string): string[] {
  return Object.keys(pkgs[id]?.currentDependencies || {}).filter(
    depId => depId !== id,
  )
}

function dependencyDepth(
  pkgs: DataModel['packageData'],
  id: string,
  depth = 0,
): number {
  return currentDeps(pkgs, id).reduce(
    (prev, depId) => dependencyDepth(pkgs, depId, prev + 1),
    depth,
  )
}
