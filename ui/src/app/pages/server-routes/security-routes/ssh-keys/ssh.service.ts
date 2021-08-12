import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { SSHKeys } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SSHService {
  private readonly keys$ = new BehaviorSubject<SSHKeys>({ })

  constructor (
    private readonly embassyApi: ApiService,
  ) { }

  watch$ () {
    return this.keys$.asObservable()
  }

  async getKeys (): Promise<void> {
    const keys = await this.embassyApi.getSshKeys({ })
    this.keys$.next(keys)
  }

  async add (pubkey: string): Promise<void> {
    const key = await this.embassyApi.addSshKey({ pubkey })
    const keys = this.keys$.getValue()
    this.keys$.next({ ...keys, ...key })
  }

  async delete (hash: string): Promise<void> {
    await this.embassyApi.deleteSshKey({ hash })
    const keys = this.keys$.getValue()

    const filtered = Object.keys(keys)
    .filter(h => h !== hash)
    .reduce((res, h) => {
      res[h] = keys[h]
      return res
    }, { })
    this.keys$.next(filtered)
  }
}
