import { Bootstrapper, DBCache } from 'patch-db-client'
import { DataModel } from './data-model'
import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap implements Bootstrapper<DataModel> {
  static CONTENT_KEY = 'patch-db-cache'

  constructor (
    private readonly storage: Storage,
  ) { }

  async init (): Promise<DBCache<DataModel>> {
    const cache = await this.storage.get(LocalStorageBootstrap.CONTENT_KEY)
    if (!cache) return { sequence: 0, data: { } as DataModel }
    return cache
  }

  async update (cache: DBCache<DataModel>): Promise<void> {
    return this.storage.set(LocalStorageBootstrap.CONTENT_KEY, cache)
  }

  async clear (): Promise<void> {
    return this.storage.remove(LocalStorageBootstrap.CONTENT_KEY)
  }
}
