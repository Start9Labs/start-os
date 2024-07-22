import { Observable } from 'rxjs'
import { MarketplacePkg, Marketplace, StoreData, StoreIdentity } from '../types'
import { T } from '@start9labs/start-sdk'

export abstract class AbstractMarketplaceService {
  abstract getKnownHosts$(): Observable<StoreIdentity[]>

  abstract getSelectedHost$(): Observable<StoreIdentity>

  abstract getMarketplace$(): Observable<Marketplace>

  abstract getSelectedStore$(): Observable<StoreData>

  abstract getPackage$(
    id: string,
    version: string | null,
    flavor: string | null,
    url?: string,
  ): Observable<MarketplacePkg> // could be {} so need to check in show page

  abstract getReleaseNotes$(
    id: string,
  ): Observable<Record<string, T.PackageInfoShort>>

  abstract fetchStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string>
}
