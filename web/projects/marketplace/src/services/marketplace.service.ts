import { Observable } from 'rxjs'
import { MarketplacePkg, StoreData } from '../types'

export abstract class AbstractMarketplaceService {
  abstract getSelectedRegistry$(): Observable<StoreData & { url?: string }>

  abstract getSelectedRegistryWithCategories$(): Observable<
    StoreData & { url: string }
  >

  abstract getStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string>
}
