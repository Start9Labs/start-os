import { PrimaryStatus, StatusRendering } from '../types/status'

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
