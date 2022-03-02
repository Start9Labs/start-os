import { Observable } from 'rxjs'
import { MarketplacePkg } from '../types/marketplace-pkg'
import { MarketplaceData } from '../types/marketplace-data'
import { Marketplace } from '../types/marketplace'

export abstract class AbstractMarketplaceService {
  pkgs: MarketplacePkg[] = []
  data?: MarketplaceData
  marketplace?: Marketplace

  abstract load(): Promise<void>

  abstract install(id: string, version?: string): Promise<void>

  abstract getPkg(id: string, version: string): Promise<MarketplacePkg>

  abstract getReleaseNotes(id: string): Observable<Record<string, string>>

  abstract getCategories(): Observable<string[]>

  abstract getPackages(): Observable<MarketplacePkg[]>
}
