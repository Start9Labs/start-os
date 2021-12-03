import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  DependencyStatus,
  HealthStatus,
  PrimaryRendering,
  renderPkgStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'
import { isEmptyObject } from './misc.util'
import {
  packageLoadingProgress,
  ProgressData,
} from './package-loading-progress'
import { Subscription } from 'rxjs'

export function getPackageInfo (entry: PackageDataEntry): PkgInfo {
  const statuses = renderPkgStatus(entry)

  return {
    entry,
    primaryRendering: PrimaryRendering[statuses.primary],
    installProgress: !isEmptyObject(entry['install-progress'])
      ? packageLoadingProgress(entry['install-progress'])
      : undefined,
    error:
      statuses.health === HealthStatus.Failure ||
      statuses.dependency === DependencyStatus.Warning,
  }
}

export interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  installProgress: ProgressData
  error: boolean
  sub?: Subscription | null
}
