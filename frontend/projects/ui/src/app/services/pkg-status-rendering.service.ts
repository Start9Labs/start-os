import { isEmptyObject } from '@start9labs/shared'
import { PackageState } from 'src/app/types/package-state'
import {
  PackageDataEntry,
  PackageMainStatus,
  Status,
} from 'src/app/services/patch-db/data-model'

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
  if (status.main.status !== PackageMainStatus.Running || !status.main.health) {
    return
  }

  const values = Object.values(status.main.health)

  if (values.some(h => h.result === 'failure')) {
    return HealthStatus.Failure
  }

  if (values.some(h => h.result === 'starting')) {
    return HealthStatus.Starting
  }

  if (values.some(h => h.result === 'loading')) {
    return HealthStatus.Loading
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
  [PrimaryStatus.NeedsConfig]: { display: 'Needs Config', color: 'warning' },
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
