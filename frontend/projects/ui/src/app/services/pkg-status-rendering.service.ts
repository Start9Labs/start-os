import {
  isEmptyObject,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  PrimaryStatus,
  StatusRendering,
  Status,
} from '@start9labs/shared'

export interface PackageStatus {
  primary: PrimaryStatus
  dependency: DependencyStatus | null
  health: HealthStatus | null
}

export function renderPkgStatus(pkg: PackageDataEntry): PackageStatus {
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

function getPrimaryStatus(status: Status): PrimaryStatus {
  if (!status.configured) {
    return PrimaryStatus.NeedsConfig
  } else {
    return status.main.status as any as PrimaryStatus
  }
}

function getDependencyStatus(pkg: PackageDataEntry): DependencyStatus {
  const installed = pkg.installed
  if (isEmptyObject(installed['current-dependencies'])) return null

  const depErrors = installed.status['dependency-errors']
  const depIds = Object.keys(depErrors).filter(key => !!depErrors[key])

  return depIds.length ? DependencyStatus.Warning : DependencyStatus.Satisfied
}

function getHealthStatus(status: Status): HealthStatus {
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

export enum DependencyStatus {
  Warning = 'warning',
  Satisfied = 'satisfied',
}

export enum HealthStatus {
  Failure = 'failure',
  Starting = 'starting',
  Loading = 'loading',
  Healthy = 'healthy',
}

export const DependencyRendering: Record<string, StatusRendering> = {
  [DependencyStatus.Warning]: { display: 'Issue', color: 'warning' },
  [DependencyStatus.Satisfied]: { display: 'Satisfied', color: 'success' },
}

export const HealthRendering: Record<string, StatusRendering> = {
  [HealthStatus.Failure]: { display: 'Failure', color: 'danger' },
  [HealthStatus.Starting]: { display: 'Starting', color: 'primary' },
  [HealthStatus.Loading]: { display: 'Loading', color: 'primary' },
  [HealthStatus.Healthy]: { display: 'Healthy', color: 'success' },
}
