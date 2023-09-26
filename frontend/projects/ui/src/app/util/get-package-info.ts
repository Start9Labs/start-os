import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  DependencyStatus,
  HealthStatus,
  PrimaryRendering,
  PrimaryStatus,
  renderPkgStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'
import { ProgressData } from 'src/app/types/progress-data'
import { Subscription } from 'rxjs'
import { packageLoadingProgress } from './package-loading-progress'
import { PackageDependencyErrors } from '../services/dep-error.service'

export function getPackageInfo(
  entry: PackageDataEntry,
  depErrors: PackageDependencyErrors,
): PkgInfo {
  const statuses = renderPkgStatus(entry, depErrors)
  const primaryRendering = PrimaryRendering[statuses.primary]

  return {
    entry,
    primaryRendering,
    primaryStatus: statuses.primary,
    installProgress: packageLoadingProgress(entry['install-progress']),
    error:
      statuses.health === HealthStatus.Failure ||
      statuses.dependency === DependencyStatus.Warning,
    warning: statuses.primary === PrimaryStatus.NeedsConfig,
    transitioning:
      primaryRendering.showDots ||
      statuses.health === HealthStatus.Waiting ||
      statuses.health === HealthStatus.Loading ||
      statuses.health === HealthStatus.Starting,
  }
}

export interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  primaryStatus: PrimaryStatus
  installProgress: ProgressData | null
  error: boolean
  warning: boolean
  transitioning: boolean
  sub?: Subscription | null
}
