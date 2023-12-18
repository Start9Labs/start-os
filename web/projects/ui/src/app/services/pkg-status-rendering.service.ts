import {
  PackageDataEntry,
  PackageMainStatus,
  PackagePlus,
  PackageState,
  Status,
} from 'src/app/services/patch-db/data-model'
import { PkgDependencyErrors } from './dep-error.service'

export interface PackageStatus {
  primary: PrimaryStatus | PackageState | PackageMainStatus
  dependency: DependencyStatus | null
  health: HealthStatus | null
}

export function renderPkgStatus(
  pkg: PackageDataEntry,
  depErrors: PkgDependencyErrors,
): PackageStatus {
  let primary: PrimaryStatus | PackageState | PackageMainStatus
  let dependency: DependencyStatus | null = null
  let health: HealthStatus | null = null

  if (pkg.state === PackageState.Installed && pkg.installed) {
    primary = getPrimaryStatus(pkg.installed.status)
    dependency = getDependencyStatus(depErrors)
    health = getHealthStatus(pkg.installed.status)
  } else {
    primary = pkg.state
  }

  return { primary, dependency, health }
}

function getPrimaryStatus(status: Status): PrimaryStatus | PackageMainStatus {
  if (!status.configured) {
    return PrimaryStatus.NeedsConfig
  } else {
    return status.main.status
  }
}

function getDependencyStatus(depErrors: PkgDependencyErrors): DependencyStatus {
  return Object.values(depErrors).some(err => !!err)
    ? DependencyStatus.Warning
    : DependencyStatus.Satisfied
}

function getHealthStatus(status: Status): HealthStatus | null {
  if (status.main.status !== PackageMainStatus.Running) {
    return null
  }

  const values = Object.values(status.main.health)

  if (values.some(h => h.result === 'failure')) {
    return HealthStatus.Failure
  }

  if (values.some(h => h.result === 'loading')) {
    return HealthStatus.Loading
  }

  if (values.some(h => !h.result || h.result === 'starting')) {
    return HealthStatus.Starting
  }

  return HealthStatus.Healthy
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
  Restarting = 'restarting',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  // config
  NeedsConfig = 'needs-config',
}

export enum DependencyStatus {
  Warning = 'warning',
  Satisfied = 'satisfied',
}

export enum HealthStatus {
  Failure = 'failure',
  Waiting = 'waiting',
  Starting = 'starting',
  Loading = 'loading',
  Healthy = 'healthy',
}

export const PrimaryRendering: Record<string, StatusRendering> = {
  [PrimaryStatus.Installing]: {
    display: 'Installing',
    color: 'primary',
    showDots: true,
  },
  [PrimaryStatus.Updating]: {
    display: 'Updating',
    color: 'primary',
    showDots: true,
  },
  [PrimaryStatus.Removing]: {
    display: 'Removing',
    color: 'danger',
    showDots: true,
  },
  [PrimaryStatus.Restoring]: {
    display: 'Restoring',
    color: 'primary',
    showDots: true,
  },
  [PrimaryStatus.Stopping]: {
    display: 'Stopping',
    color: 'dark-shade',
    showDots: true,
  },
  [PrimaryStatus.Restarting]: {
    display: 'Restarting',
    color: 'tertiary',
    showDots: true,
  },
  [PrimaryStatus.Stopped]: {
    display: 'Stopped',
    color: 'dark-shade',
    showDots: false,
  },
  [PrimaryStatus.BackingUp]: {
    display: 'Backing Up',
    color: 'primary',
    showDots: true,
  },
  [PrimaryStatus.Starting]: {
    display: 'Starting',
    color: 'primary',
    showDots: true,
  },
  [PrimaryStatus.Running]: {
    display: 'Running',
    color: 'success',
    showDots: false,
  },
  [PrimaryStatus.NeedsConfig]: {
    display: 'Needs Config',
    color: 'warning',
    showDots: false,
  },
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