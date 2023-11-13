import { Bootstrapper, DBCache } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Injectable } from '@angular/core'
import { StorageService } from '../storage.service'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap implements Bootstrapper<DataModel> {
  static CONTENT_KEY = 'patch-db-cache'

  constructor(private readonly storage: StorageService) {}

  init(): DBCache<DataModel> {
    const cache = this.storage.get<DBCache<DataModel>>(
      LocalStorageBootstrap.CONTENT_KEY,
    )

    return cache || { sequence: 0, data: {} as DataModel }
  }

  update(cache: DBCache<DataModel>): void {
    this.storage.set(LocalStorageBootstrap.CONTENT_KEY, cache)
  }
}
