import { Observable } from 'rxjs'
import {
  Marketplace,
  MarketplacePkg,
  StoreData,
  StoreIdentity,
  StoreIdentityWithData,
} from '../types'

export abstract class AbstractMarketplaceService {
  abstract getKnownHosts$(): Observable<StoreIdentity[]>

  abstract getSelectedHost$(): Observable<StoreIdentity>

  abstract getMarketplace$(): Observable<Marketplace>

  abstract getSelectedStore$(): Observable<StoreData>

  abstract getSelectedStoreWithCategories$(): Observable<StoreIdentityWithData>

  abstract getPackage$(
    id: string,
    version: string | null,
    flavor: string | null,
    url?: string,
  ): Observable<MarketplacePkg>

  abstract fetchStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string>
}
