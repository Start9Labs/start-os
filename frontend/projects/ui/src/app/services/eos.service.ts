import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { MarketplaceEOS } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Emver } from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class EOSService {
  eos: MarketplaceEOS
  updateAvailable$ = new BehaviorSubject<boolean>(false)

  constructor(
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
  ) {}

  async getEOS(): Promise<boolean> {
    this.eos = await this.api.getEos({
      'eos-version-compat':
        this.patch.getData()['server-info']['eos-version-compat'],
    })
    const updateAvailable =
      this.emver.compare(
        this.eos.version,
        this.patch.getData()['server-info'].version,
      ) === 1
    this.updateAvailable$.next(updateAvailable)
    return updateAvailable
  }
}
