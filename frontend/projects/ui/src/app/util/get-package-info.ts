import {
  DependencyStatus,
  HealthStatus,
  renderPkgStatus,
} from '../services/pkg-status-rendering.service'
import {
  packageLoadingProgress,
  PackageDataEntry,
  ProgressData,
  PrimaryRendering,
  StatusRendering,
} from '@start9labs/shared'
import { Subscription } from 'rxjs'

export function getPackageInfo(entry: PackageDataEntry): PkgInfo {
  const statuses = renderPkgStatus(entry)

  return {
    entry,
    primaryRendering: PrimaryRendering[statuses.primary],
    installProgress: packageLoadingProgress(entry['install-progress']),
    error:
      statuses.health === HealthStatus.Failure ||
      statuses.dependency === DependencyStatus.Warning,
  }
}

export interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  installProgress: ProgressData | null
  error: boolean
  sub?: Subscription | null
}
