import { Bootstrapper, DBCache } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap implements Bootstrapper<DataModel> {
  static CONTENT_KEY = 'patch-db-cache'

  constructor(private readonly storage: Storage) {}

  async init(): Promise<DBCache<DataModel>> {
    const cache: DBCache<DataModel> = await this.storage.get(
      LocalStorageBootstrap.CONTENT_KEY,
    )
    return cache || { sequence: 0, data: {} as DataModel }
  }

  async update(cache: DBCache<DataModel>): Promise<void> {
    await this.storage.set(LocalStorageBootstrap.CONTENT_KEY, cache)
  }
}
