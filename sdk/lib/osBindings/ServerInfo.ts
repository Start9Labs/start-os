// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { Governor } from "./Governor"
import type { IpInfo } from "./IpInfo"
import type { ServerStatus } from "./ServerStatus"
import type { WifiInfo } from "./WifiInfo"

export type ServerInfo = {
  arch: string
  platform: string
  id: string
  hostname: string
  version: string
  lastBackup: string | null
  eosVersionCompat: string
  lanAddress: string
  onionAddress: string
  /**
   * for backwards compatibility
   */
  torAddress: string
  ipInfo: { [key: string]: IpInfo }
  statusInfo: ServerStatus
  wifi: WifiInfo
  unreadNotificationCount: number
  passwordHash: string
  pubkey: string
  caFingerprint: string
  ntpSynced: boolean
  zram: boolean
  governor: Governor | null
}
