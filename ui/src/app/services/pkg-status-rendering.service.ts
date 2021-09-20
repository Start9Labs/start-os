import { isEmptyObject } from '../util/misc.util'
import { PackageDataEntry, InstalledPackageDataEntry, PackageMainStatus, PackageState, Status } from './patch-db/data-model'

export function renderPkgStatus (pkg: PackageDataEntry): {
  primary: PrimaryStatus,
  dependency: DependencyStatus | null,
  health: HealthStatus | null
} {
  let primary: PrimaryStatus
  let dependency: DependencyStatus | null = null
  let health: HealthStatus | null = null

  if (pkg.state === PackageState.Installed) {
    primary = PrimaryStatus[pkg.installed.status.main.status]
    dependency = getDependencyStatus(pkg.installed)
    health = getHealthStatus(pkg.installed.status)
  } else {
    primary = PrimaryStatus[pkg.state]
  }

  return { primary, dependency, health }
}

function getDependencyStatus (pkg: InstalledPackageDataEntry): DependencyStatus {
  if (isEmptyObject(pkg['current-dependencies'])) return null

  const pkgIds = Object.keys(pkg.status['dependency-errors'])

  for (let pkgId of pkgIds) {
    if (pkg.manifest.dependencies[pkgId].critical) {
      return DependencyStatus.Critical
    }
  }

  return pkgIds.length ? DependencyStatus.Issue : DependencyStatus.Satisfied
}

function getHealthStatus (status: Status): HealthStatus {
  if (!status.configured) {
    return HealthStatus.NeedsConfig
  }

  if (status.main.status === PackageMainStatus.Running) {
    const values = Object.values(status.main.health)
    if (values.some(h => h.result === 'failure')) {
      return HealthStatus.Failure
    } else if (values.some(h => h.result === 'starting')) {
      return HealthStatus.Starting
    } else if (values.some(h => h.result === 'loading')) {
      return HealthStatus.Loading
    } else {
      return HealthStatus.Healthy
    }
  }
}

export interface StatusRendering {
  display: string
  color: string
  showDots?: boolean
}

export enum PrimaryStatus {
  // state
  Installing = 'installing',
  Updating = 'updating',
  Removing = 'removing',
  // status
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  Restoring = 'restoring',
}

export enum DependencyStatus {
  Issue = 'issue',
  Critical = 'critical',
  Satisfied = 'satisfied',
}

export enum HealthStatus {
  NeedsConfig = 'needs-config',
  Failure = 'failure',
  Starting = 'starting',
  Loading = 'loading',
  Healthy = 'healthy',
}

export const PrimaryRendering: { [key: string]: StatusRendering } = {
  [PrimaryStatus.Installing]: { display: 'Installing', color: 'primary', showDots: true },
  [PrimaryStatus.Updating]: { display: 'Updating', color: 'primary', showDots: true },
  [PrimaryStatus.Removing]: { display: 'Removing', color: 'warning', showDots: true },
  [PrimaryStatus.Stopping]: { display: 'Stopping', color: 'dark', showDots: true },
  [PrimaryStatus.Stopped]: { display: 'Stopped', color: 'dark', showDots: false },
  [PrimaryStatus.BackingUp]: { display: 'Backing Up', color: 'warning', showDots: true },
  [PrimaryStatus.Restoring]: { display: 'Restoring', color: 'primary', showDots: true },
  [PrimaryStatus.Running]: { display: 'Running', color: 'success', showDots: false },
}

export const DependencyRendering: { [key: string]: StatusRendering }  = {
  [DependencyStatus.Issue]: { display: 'Issue', color: 'warning' },
  [DependencyStatus.Critical]: { display: 'Critical Issue', color: 'danger' },
  [DependencyStatus.Satisfied]: { display: 'Satisfied', color: 'success' },
}

export const HealthRendering: { [key: string]: StatusRendering } = {
  [HealthStatus.NeedsConfig]: { display: 'Needs Config', color: 'warning' },
  [HealthStatus.Failure]: { display: 'Failure', color: 'danger' },
  [HealthStatus.Starting]: { display: 'Starting', color: 'primary' },
  [HealthStatus.Loading]: { display: 'Loading', color: 'primary' },
  [HealthStatus.Healthy]: { display: 'Healthy', color: 'success' },
}
