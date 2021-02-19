import { BootstrapCache } from 'patch-db-client';
import { DataModel } from './data-model';
import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage'
@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap implements BootstrapCache<DataModel> {
  static CONTENT_KEY = 'patch-db-cache'

  constructor (private readonly storage: Storage) { }
  async commitCache (sequence: number, cache: DataModel): Promise<void> {
    return this.storage.set(LocalStorageBootstrap.CONTENT_KEY, { sequence, cache })
  }

  async nukeCache (): Promise<void> {
    return this.storage.remove(LocalStorageBootstrap.CONTENT_KEY)
  }

  restoreCache (): Promise<{ sequence: number; cache: DataModel }> {
    return this.storage.get(LocalStorageBootstrap.CONTENT_KEY).then(drudged => drudged || { sequence: 0, cache: { } })
  }

  init (): Promise<void> {
    return this.storage.ready().then(() => { })
  }
}
