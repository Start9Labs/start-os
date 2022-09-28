import { Inject, Injectable } from '@angular/core'
import { DOCUMENT } from '@angular/common'

const PREFIX = '_embassystorage/_embassykv/'

@Injectable({
  providedIn: 'root',
})
export class StorageService {
  private readonly storage = this.document.defaultView!.localStorage

  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  get<T>(key: string): T {
    return JSON.parse(String(this.storage.getItem(`${PREFIX}${key}`)))
  }

  set<T>(key: string, value: T) {
    this.storage.setItem(`${PREFIX}${key}`, JSON.stringify(value))
  }

  clear() {
    Array.from(
      { length: this.storage.length },
      (_, i) => this.storage.key(i) || '',
    )
      .filter(key => key.startsWith(PREFIX))
      .forEach(key => this.storage.removeItem(key))
  }
}
