import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  DependencyStatus,
  HealthStatus,
  PrimaryRendering,
  renderPkgStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'
import { ProgressData } from 'src/app/types/progress-data'
import { Subscription } from 'rxjs'
import { packageLoadingProgress } from './package-loading-progress'

export function getPackageInfo(entry: PackageDataEntry): PkgInfo {
  const statuses = renderPkgStatus(entry)
  const primaryRendering = PrimaryRendering[statuses.primary]

  return {
    entry,
    primaryRendering,
    installProgress: packageLoadingProgress(entry['install-progress']),
    error:
      statuses.health === HealthStatus.Failure ||
      statuses.dependency === DependencyStatus.Warning,
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
  installProgress: ProgressData | null
  error: boolean
  transitioning: boolean
  sub?: Subscription | null
}
