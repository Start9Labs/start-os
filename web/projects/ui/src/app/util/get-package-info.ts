import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  DependencyStatus,
  HealthStatus,
  PrimaryRendering,
  PrimaryStatus,
  renderPkgStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'
import { Subscription } from 'rxjs'
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
  error: boolean
  warning: boolean
  transitioning: boolean
  sub?: Subscription | null
}
