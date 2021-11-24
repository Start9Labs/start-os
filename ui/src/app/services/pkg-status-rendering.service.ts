import { isEmptyObject } from '../util/misc.util'
import { PackageDataEntry, PackageMainStatus, PackageState, Status } from './patch-db/data-model'

export function renderPkgStatus (pkg: PackageDataEntry): {
  primary: PrimaryStatus,
  dependency: DependencyStatus | null,
  health: HealthStatus | null
} {
  let primary: PrimaryStatus
  let dependency: DependencyStatus | null = null
  let health: HealthStatus | null = null

  if (pkg.state === PackageState.Installed) {
    primary = getPrimaryStatus(pkg.installed.status)
    dependency = getDependencyStatus(pkg)
    health = getHealthStatus(pkg.installed.status)
  } else {
    primary = pkg.state as string as PrimaryStatus
  }

  return { primary, dependency, health }
}

function getPrimaryStatus (status: Status): PrimaryStatus {
  if (!status.configured) {
    return PrimaryStatus.NeedsConfig
  } else {
    return status.main.status as any as PrimaryStatus
  }
}

function getDependencyStatus (pkg: PackageDataEntry): DependencyStatus {
  const installed = pkg.installed
  if (isEmptyObject(installed['current-dependencies'])) return null

  const depErrors = installed.status['dependency-errors']
  const depIds = Object.keys(depErrors).filter(key => !!depErrors[key])

  for (let pkgId of depIds) {
    if (pkg.manifest.dependencies[pkgId].critical) {
      return DependencyStatus.Critical
    }
  }

  return depIds.length ? DependencyStatus.Issue : DependencyStatus.Satisfied
}

function getHealthStatus (status: Status): HealthStatus {
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
  Restoring = 'restoring',
  // status
  Starting = 'starting',
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  // config
  NeedsConfig = 'needs-config',
}

export enum DependencyStatus {
  Issue = 'issue',
  Critical = 'critical',
  Satisfied = 'satisfied',
}

export enum HealthStatus {
  Failure = 'failure',
  Starting = 'starting',
  Loading = 'loading',
  Healthy = 'healthy',
}

export const PrimaryRendering: { [key: string]: StatusRendering } = {
  [PrimaryStatus.Installing]: { display: 'Installing', color: 'primary', showDots: true },
  [PrimaryStatus.Updating]: { display: 'Updating', color: 'primary', showDots: true },
  [PrimaryStatus.Removing]: { display: 'Removing', color: 'danger', showDots: true },
  [PrimaryStatus.Restoring]: { display: 'Restoring', color: 'primary', showDots: true },
  [PrimaryStatus.Stopping]: { display: 'Stopping', color: 'dark-shade', showDots: true },
  [PrimaryStatus.Stopped]: { display: 'Stopped', color: 'dark-shade', showDots: false },
  [PrimaryStatus.BackingUp]: { display: 'Backing Up', color: 'primary', showDots: true },
  [PrimaryStatus.Starting]: { display: 'Starting', color: 'primary', showDots: true },
  [PrimaryStatus.Running]: { display: 'Running', color: 'success', showDots: false },
  [PrimaryStatus.NeedsConfig]: { display: 'Needs Config', color: 'warning' },
}

export const DependencyRendering: { [key: string]: StatusRendering }  = {
  [DependencyStatus.Issue]: { display: 'Issue', color: 'warning' },
  [DependencyStatus.Critical]: { display: 'Critical Issue', color: 'danger' },
  [DependencyStatus.Satisfied]: { display: 'Satisfied', color: 'success' },
}

export const HealthRendering: { [key: string]: StatusRendering } = {
  [HealthStatus.Failure]: { display: 'Failure', color: 'danger' },
  [HealthStatus.Starting]: { display: 'Starting', color: 'primary' },
  [HealthStatus.Loading]: { display: 'Loading', color: 'primary' },
  [HealthStatus.Healthy]: { display: 'Healthy', color: 'success' },
}
