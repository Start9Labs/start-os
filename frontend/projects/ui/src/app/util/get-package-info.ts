import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  DependencyStatus,
  HealthStatus,
  PrimaryRendering,
  PrimaryStatus,
  renderPkgStatus,
} from '../services/pkg-status-rendering.service'
import { PkgInfo } from '../types/pkg-info'
import { packageLoadingProgress } from './package-loading-progress'
import { PkgDependencyErrors } from '../services/dep-error.service'

export function getPackageInfo(
  entry: PackageDataEntry,
  depErrors: PkgDependencyErrors,
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
