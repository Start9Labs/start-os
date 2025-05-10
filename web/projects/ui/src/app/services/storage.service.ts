import { Inject, Injectable } from '@angular/core'
import { DOCUMENT } from '@angular/common'

const PREFIX = '_startos/'

@Injectable({
  providedIn: 'root',
})
export class StorageService {
  private readonly storage = this.document.defaultView!.localStorage

  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  get<T>(key: string): T {
    return JSON.parse(String(this.storage.getItem(`${PREFIX}${key}`)))
  }

  set(key: string, value: any) {
    this.storage.setItem(`${PREFIX}${key}`, JSON.stringify(value))
  }

  clear() {
    this.storage.clear()
  }

  migrate036() {
    const oldPrefix = '_embassystorage/_embassykv/'
    if (!!this.storage.getItem(`${oldPrefix}loggedInKey`)) {
      const cache = this.storage.getItem(`${oldPrefix}patch-db-cache`)
      this.clear()
      this.set('loggedIn', true)
      this.set('patchDB', cache)
    }
  }
}
