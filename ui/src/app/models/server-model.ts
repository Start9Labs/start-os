import { Injectable } from '@angular/core'
import { Subject, BehaviorSubject } from 'rxjs'
import { PropertySubject, peekProperties, initPropertySubject } from '../util/property-subject.util'
import { AppModel } from './app-model'
import { ConfigService } from 'src/app/services/config.service'
import { Storage } from '@ionic/storage'
import { throttleTime, delay } from 'rxjs/operators'
import { StorageKeys } from './storage-keys'

@Injectable({
  providedIn: 'root',
})
export class ServerModel {
  lastUpdateTimestamp: Date
  $delta$ = new Subject<void>()
  private embassy: PropertySubject<S9Server>

  constructor (
    private readonly storage: Storage,
    private readonly appModel: AppModel,
    private readonly config: ConfigService,
  ) {
    this.embassy = this.defaultEmbassy()
    this.$delta$.pipe(
      throttleTime(500), delay(500),
    ).subscribe(() => {
      this.commitCache()
    })
  }

  // client fxns
  watch (): PropertySubject<S9Server> {
    return this.embassy
  }

  peek (): S9Server {
    return peekProperties(this.embassy)
  }

  update (update: Partial<S9Server>, timestamp: Date = new Date()): void {
    if (this.lastUpdateTimestamp > timestamp) return

    if (update.versionInstalled && (update.versionInstalled !== this.config.version) && this.embassy.status.getValue() === ServerStatus.RUNNING) {
      console.log('update detected, force reload page')
      this.clear()
      this.nukeCache().then(
        () => location.replace('?upd=' + new Date()),
      )
    }

    Object.entries(update).forEach(
      ([key, value]) => {
        if (!this.embassy[key]) {
          console.warn('Received an unexpected key: ', key)
          this.embassy[key] = new BehaviorSubject(value)
        } else if (JSON.stringify(this.embassy[key].getValue()) !== JSON.stringify(value)) {
          this.embassy[key].next(value)
        }
      },
    )
    this.$delta$.next()
    this.lastUpdateTimestamp = timestamp
  }

  // cache mgmt
  clear () {
    this.update(peekProperties(this.defaultEmbassy()))
  }

  private commitCache (): Promise<void> {
    return this.storage.set(StorageKeys.SERVER_CACHE_KEY, peekProperties(this.embassy))
  }

  private nukeCache (): Promise<void> {
    return this.storage.remove(StorageKeys.SERVER_CACHE_KEY)
  }

  async restoreCache (): Promise<void> {
    const emb = await this.storage.get(StorageKeys.SERVER_CACHE_KEY)
    if (emb && emb.versionInstalled === this.config.version) this.update(emb)
  }

  // server state change
  markUnreachable (): void {
    this.update({ status: ServerStatus.UNREACHABLE })
    this.appModel.markAppsUnreachable()
  }

  markUnknown (): void {
    this.update({ status: ServerStatus.UNKNOWN })
    this.appModel.markAppsUnknown()
  }

  defaultEmbassy (): PropertySubject<S9Server> {
    return initPropertySubject({
      serverId: undefined,
      name: undefined,
      origin: this.config.origin,
      versionInstalled: undefined,
      versionLatest: undefined,
      status: ServerStatus.UNKNOWN,
      badge: 0,
      alternativeRegistryUrl: undefined,
      specs: { },
      wifi: { ssids: [], current: undefined },
      ssh: [],
      notifications: [],
    })
  }
}

export interface S9Server {
  serverId: string
  name: string
  origin: string
  versionInstalled: string
  versionLatest: string | undefined
  status: ServerStatus
  badge: number
  alternativeRegistryUrl: string | null
  specs: ServerSpecs
  wifi: { ssids: string[], current: string }
  ssh: SSHFingerprint[]
  notifications: S9Notification[]
}

export interface S9Notification {
  id: string
  appId: string
  createdAt: string
  code: string
  title: string
  message: string
}

export interface ServerSpecs {
  [key: string]: string | number
}

export interface ServerMetrics {
  [key: string]: {
    [key: string]: {
      value: string | number | null
      unit?: string
    }
  }
}

export interface SSHFingerprint {
  alg: string
  hash: string
  hostname: string
}

export interface DiskInfo {
  logicalname: string,
  size: string,
  description: string | null,
  partitions: DiskPartition[]
}

export interface DiskPartition {
  logicalname: string,
  isMounted: boolean, // We do not allow backups to mounted partitions
  size: string | null,
  label: string | null,
}

export enum ServerStatus {
  UNKNOWN = 'UNKNOWN',
  UNREACHABLE = 'UNREACHABLE',
  UPDATING = 'UPDATING',
  NEEDS_CONFIG = 'NEEDS_CONFIG',
  RUNNING = 'RUNNING',
}
