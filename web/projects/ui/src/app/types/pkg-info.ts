import { Subscription } from 'rxjs'
import {
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from '../services/patch-db/data-model'
import {
  PrimaryStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'

export interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  primaryStatus: PrimaryStatus | PackageState | PackageMainStatus
  error: boolean
  warning: boolean
  transitioning: boolean
  sub?: Subscription | null
}
