import { Subscription } from 'rxjs'
import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  PrimaryStatus,
  StatusRendering,
} from '../services/pkg-status-rendering.service'

export interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  primaryStatus: PrimaryStatus
  error: boolean
  warning: boolean
  transitioning: boolean
  sub?: Subscription | null
}
