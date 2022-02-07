import { NotificationData } from './notification-data'
import { NotificationLevel } from './notification-level'

export type ServerNotifications = ServerNotification<any>[]

export interface ServerNotification<T extends number> {
  id: number
  'package-id': string | null
  'created-at': string
  code: T
  level: NotificationLevel
  title: string
  message: string
  data: NotificationData<T>
}
