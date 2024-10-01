import { Dump } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Injectable } from '@angular/core'
import { StorageService } from '../storage.service'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageBootstrap {
  static CONTENT_KEY = 'patchDB'

  constructor(private readonly storage: StorageService) {}

  init(): Dump<DataModel> {
    const cache = this.storage.get<DataModel>(LocalStorageBootstrap.CONTENT_KEY)

    return cache ? { id: 1, value: cache } : { id: 0, value: {} as DataModel }
  }

  update(cache: DataModel): void {
    this.storage.set(LocalStorageBootstrap.CONTENT_KEY, cache)
  }
}
