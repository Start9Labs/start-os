import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PkgDependencyErrors } from './dep-error.service'
import { T } from '@start9labs/start-sdk'

export interface PackageStatus {
  primary: PrimaryStatus
  dependency: DependencyStatus | null
  health: T.HealthStatus | null
}

export function renderPkgStatus(
  pkg: PackageDataEntry,
  depErrors: PkgDependencyErrors,
): PackageStatus {
  let primary: PrimaryStatus
  let dependency: DependencyStatus | null = null
  let health: T.HealthStatus | null = null

  if (pkg.stateInfo.state === 'installed') {
    primary = getInstalledPrimaryStatus(pkg.status)
    dependency = getDependencyStatus(depErrors)
    health = getHealthStatus(pkg.status)
  } else {
    primary = pkg.stateInfo.state
  }

  return { primary, dependency, health }
}

function getInstalledPrimaryStatus(status: T.Status): PrimaryStatus {
  if (!status.configured) {
    return 'needsConfig'
  } else {
    return status.main.status
  }
}

function getDependencyStatus(depErrors: PkgDependencyErrors): DependencyStatus {
  return Object.values(depErrors).some(err => !!err) ? 'warning' : 'satisfied'
}

function getHealthStatus(status: T.Status): T.HealthStatus | null {
  if (status.main.status !== 'running' || !status.main.health) {
    return null
  }

  const values = Object.values(status.main.health)

  if (values.some(h => h.result === 'failure')) {
    return 'failure'
  }

  if (values.some(h => h.result === 'loading')) {
    return 'loading'
  }

  if (values.some(h => h.result === 'starting')) {
    return 'starting'
  }

  return 'success'
}

export interface StatusRendering {
  display: string
  color: string
  showDots?: boolean
}

export type PrimaryStatus =
  | 'installing'
  | 'updating'
  | 'removing'
  | 'restoring'
  | 'starting'
  | 'running'
  | 'stopping'
  | 'restarting'
  | 'stopped'
  | 'backingUp'
  | 'needsConfig'

export type DependencyStatus = 'warning' | 'satisfied'

export const PrimaryRendering: Record<PrimaryStatus, StatusRendering> = {
  installing: {
    display: 'Installing',
    color: 'primary',
    showDots: true,
  },
  updating: {
    display: 'Updating',
    color: 'primary',
    showDots: true,
  },
  removing: {
    display: 'Removing',
    color: 'danger',
    showDots: true,
  },
  restoring: {
    display: 'Restoring',
    color: 'primary',
    showDots: true,
  },
  stopping: {
    display: 'Stopping',
    color: 'dark-shade',
    showDots: true,
  },
  restarting: {
    display: 'Restarting',
    color: 'tertiary',
    showDots: true,
  },
  stopped: {
    display: 'Stopped',
    color: 'dark-shade',
    showDots: false,
  },
  backingUp: {
    display: 'Backing Up',
    color: 'primary',
    showDots: true,
  },
  starting: {
    display: 'Starting',
    color: 'primary',
    showDots: true,
  },
  running: {
    display: 'Running',
    color: 'success',
    showDots: false,
  },
  needsConfig: {
    display: 'Needs Config',
    color: 'warning',
    showDots: false,
  },
}

export const DependencyRendering: Record<DependencyStatus, StatusRendering> = {
  warning: { display: 'Issue', color: 'warning' },
  satisfied: { display: 'Satisfied', color: 'success' },
}

export const HealthRendering: Record<T.HealthStatus, StatusRendering> = {
  failure: { display: 'Failure', color: 'danger' },
  starting: { display: 'Starting', color: 'primary' },
  loading: { display: 'Loading', color: 'primary' },
  success: { display: 'Healthy', color: 'success' },
  disabled: { display: 'Disabled', color: 'dark' },
}
