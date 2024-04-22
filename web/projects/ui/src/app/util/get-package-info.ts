import { PackageDataEntry } from '../services/patch-db/data-model'
import {
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
    error: statuses.health === 'failure' || statuses.dependency === 'warning',
    warning: statuses.primary === 'needsConfig',
    transitioning:
      primaryRendering.showDots ||
      statuses.health === 'loading' ||
      statuses.health === 'starting',
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
