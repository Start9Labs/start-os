import { Observable } from 'rxjs'
import { MarketplaceInfo } from '../types/marketplace-info'
import { MarketplacePkg } from '../types/marketplace-pkg'

export abstract class AbstractMarketplaceService {
  abstract getMarketplaceInfo$(): Observable<MarketplaceInfo>

  abstract getPackages$(): Observable<MarketplacePkg[]>

  abstract getPackage(
    id: string,
    version: string,
    url?: string,
  ): Observable<MarketplacePkg | undefined>

  abstract fetchReleaseNotes(
    id: string,
    url?: string,
  ): Observable<Record<string, string>>

  abstract fetchPackageMarkdown(
    id: string,
    type: string,
    url?: string,
  ): Observable<string>
}
