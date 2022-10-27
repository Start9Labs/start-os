import { Observable } from 'rxjs'
import {
  MarketplacePkg,
  Marketplace,
  MarketplaceURL,
  MarketplaceName,
  StoreData,
} from '../types'

export abstract class AbstractMarketplaceService {
  abstract getKnownHosts$(): Observable<Record<MarketplaceURL, MarketplaceName>>

  abstract getSelectedHost$(): Observable<{ url: string; name: string }>

  abstract getMarketplace$(): Observable<Marketplace>

  abstract getSelectedStore$(): Observable<StoreData | null>

  abstract getPackage$(
    id: string,
    version: string,
    url?: string,
  ): Observable<MarketplacePkg | undefined>

  abstract fetchReleaseNotes$(
    id: string,
    url?: string,
  ): Observable<Record<string, string>>

  abstract fetchStatic$(
    id: string,
    type: string,
    url?: string,
  ): Observable<string>
}
