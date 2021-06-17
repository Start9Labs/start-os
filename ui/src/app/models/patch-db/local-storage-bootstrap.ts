import { Bootstrapper, DBCache } from 'patch-db-client'
import { DataModel } from './data-model'
import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage'
import { ApiService } from 'src/app/services/api/api.service'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap implements Bootstrapper<DataModel> {
  static CONTENT_KEY = 'patch-db-cache'

  constructor (
    private readonly storage: Storage,
    private readonly apiService: ApiService,
  ) { }

  async init (): Promise<DBCache<DataModel>> {
    let cache = await this.storage.get(LocalStorageBootstrap.CONTENT_KEY) as DBCache<DataModel>
    if (!cache || cache.sequence === 0) {
      console.log('No cached data, getting dump from server')
      const { id, value } = await this.apiService.getDump()
      cache.sequence = id
      cache.data = value
      await this.update(cache)
    }
    return cache
  }

  async update (cache: DBCache<DataModel>): Promise<void> {
    return this.storage.set(LocalStorageBootstrap.CONTENT_KEY, cache)
  }
}
