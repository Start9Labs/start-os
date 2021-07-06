import { HealthCheckResultLoading, MainStatusRunning, PackageMainStatus, PackageState, Status } from './patch-db/data-model'

export function renderPkgStatus (state: PackageState, status: Status): PkgStatusRendering {
  switch (state) {
    case PackageState.Installing: return { display: 'Installing', color: 'primary', showDots: true, feStatus: FEStatus.Installing }
    case PackageState.Updating: return { display: 'Updating', color: 'primary', showDots: true, feStatus: FEStatus.Updating }
    case PackageState.Removing: return { display: 'Removing', color: 'warning', showDots: true, feStatus: FEStatus.Removing }
    case PackageState.Installed: return handleInstalledState(status)
  }
}

function handleInstalledState (status: Status): PkgStatusRendering {
  if (!status.configured) {
    return { display: 'Needs Config', color: 'warning', showDots: false, feStatus: FEStatus.NeedsConfig }
  }

  if (Object.keys(status['dependency-errors']).length) {
    return { display: 'Dependency Issue', color: 'warning', showDots: false, feStatus: FEStatus.DependencyIssue }
  }

  switch (status.main.status) {
    case PackageMainStatus.Stopping: return { display: 'Stopping', color: 'dark', showDots: true, feStatus: FEStatus.Stopping }
    case PackageMainStatus.Stopped: return { display: 'Stopped', color: 'medium', showDots: false, feStatus: FEStatus.Stopped }
    case PackageMainStatus.BackingUp: return { display: 'Backing Up', color: 'warning', showDots: true, feStatus: FEStatus.BackingUp }
    case PackageMainStatus.Restoring: return { display: 'Restoring', color: 'primary', showDots: true, feStatus: FEStatus.Restoring }
    case PackageMainStatus.Running: return handleRunningState(status.main)
  }
}

function handleRunningState (status: MainStatusRunning): PkgStatusRendering {
  if (Object.values(status.health).some(h => h.result === 'failure')) {
    return { display: 'Needs Attention', color: 'danger', showDots: false, feStatus: FEStatus.NeedsAttention }
  } else if (Object.values(status.health).some(h => h.result === 'starting')) {
    return { display: 'Starting', color: 'warning', showDots: true, feStatus: FEStatus.Starting }
  } else if (Object.values(status.health).some(h => h.result === 'loading')) {
    const firstLoading = Object.values(status.health).find(h => h.result === 'loading') as HealthCheckResultLoading
    return { display: firstLoading.message, color: 'warning', showDots: true, feStatus: FEStatus.Loading }
  } else {
    return { display: 'Running', color: 'success', showDots: false, feStatus: FEStatus.Running }
  }
}

export interface PkgStatusRendering {
  feStatus: FEStatus
  display: string
  color: string
  showDots: boolean
}

// aggregate of all pkg statuses, except for Installed, which implies a "main" or "FE" status
export enum FEStatus {
  // pkg
  Installing = 'installing',
  Updating = 'updating',
  Removing = 'removing',
  // main
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  Restoring = 'restoring',
  // FE
  NeedsAttention = 'needs-attention',
  Starting = 'starting',
  Connecting = 'connecting',
  DependencyIssue = 'dependency-issue',
  NeedsConfig = 'needs-config',
  Loading = 'loading',
}
