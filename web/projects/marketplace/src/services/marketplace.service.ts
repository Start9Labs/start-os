import { Observable } from 'rxjs'
import {
  MarketplacePkg,
  Marketplace,
  StoreData,
  StoreIdentity,
  DefaultGetPackageParams,
} from '../types'
import { T } from '@start9labs/start-sdk'

export abstract class AbstractMarketplaceService {
  abstract getKnownHosts$(): Observable<StoreIdentity[]>

  abstract getSelectedHost$(): Observable<StoreIdentity>

  abstract getMarketplace$<T extends T.GetPackageParams>(): Observable<
    Marketplace<DefaultGetPackageParams>
  >

  abstract getSelectedStore$<T extends T.GetPackageParams>(): Observable<
    StoreData<T>
  >

  abstract getPackage$<T extends T.GetPackageParams>(
    params: T,
    url?: string,
  ): Observable<MarketplacePkg<T>> // could be {} so need to check in show page

  abstract fetchReleaseNotes$(
    id: string,
    url?: string,
  ): Observable<Record<string, T.PackageInfoShort>>

  abstract fetchStatic$(
    id: string,
    type: string,
    version: string,
    url: string | null,
  ): Observable<string>
}
