import { AppStatus } from 'src/app/models/app-model'
import { ServerStatus } from 'src/app/models/server-model'

export const ServerStatusRendering: {
  [k in ServerStatus]: { display: string; color: string; showDots: boolean; }
} = {
  [ServerStatus.UNKNOWN]: { display: 'Connecting', color: 'dark', showDots: true },
  [ServerStatus.UNREACHABLE]: { display: 'Unreachable', color: 'danger', showDots: false },
  [ServerStatus.NEEDS_CONFIG]: { display: 'Needs Config', color: 'warning', showDots: false },
  [ServerStatus.RUNNING]: { display: 'Connected', color: 'success', showDots: false },
  [ServerStatus.UPDATING]: { display: 'Updating', color: 'primary', showDots: true },
}

export const AppStatusRendering: {
  [k in AppStatus]: { display: string; color: string; showDots: boolean; style?: string; }
} = {
  [AppStatus.UNKNOWN]: { display: 'Connecting', color: 'dark', showDots: true },
  [AppStatus.REMOVING]: { display: 'Removing', color: 'dark', showDots: true },
  [AppStatus.CRASHED]: { display: 'Crashing', color: 'danger', showDots: true },
  [AppStatus.NEEDS_CONFIG]: { display: 'Needs Config', color: 'warning', showDots: false },
  [AppStatus.RUNNING]: { display: 'Running', color: 'success', showDots: false },
  [AppStatus.UNREACHABLE]: { display: 'Unreachable', color: 'danger', showDots: false },
  [AppStatus.STOPPED]: { display: 'Not Running', color: 'medium', showDots: false },
  [AppStatus.CREATING_BACKUP]: { display: 'Backing Up', color: 'dark', showDots: true },
  [AppStatus.RESTORING_BACKUP]: { display: 'Restoring', color: 'dark', showDots: true },
  [AppStatus.INSTALLING]: { display: 'Installing', color: 'primary', showDots: true },
  [AppStatus.DEAD]: { display: 'Dead', color: 'danger', showDots: false },
  [AppStatus.BROKEN_DEPENDENCIES]: { display: 'Dependency Issue', color: 'warning', showDots: false },
  [AppStatus.STOPPING]: { display: 'Stopping', color: 'dark', showDots: true },
  [AppStatus.RESTARTING]: { display: 'Restarting', color: 'dark', showDots: true },
}
