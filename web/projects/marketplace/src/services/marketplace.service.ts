import { Observable } from 'rxjs'
import { MarketplacePkg, StoreData } from '../types'

export abstract class AbstractMarketplaceService {
  abstract getRegistry$(): Observable<StoreData & { url?: string }>

  abstract getStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string>
}
