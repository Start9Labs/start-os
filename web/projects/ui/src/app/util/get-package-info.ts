import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  PrimaryRendering,
  renderPkgStatus,
} from '../services/pkg-status-rendering.service'
import { PkgInfo } from '../types/pkg-info'
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
    error: statuses.health === 'failure' || statuses.dependency === 'warning',
    warning: statuses.primary === 'needsConfig',
    transitioning:
      primaryRendering.showDots ||
      statuses.health === 'loading' ||
      statuses.health === 'starting',
  }
}
