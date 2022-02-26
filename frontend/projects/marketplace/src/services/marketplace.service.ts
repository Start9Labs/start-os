import { MarketplacePkg } from '../types/marketplace-pkg'
import { MarketplaceData } from '../types/marketplace-data'

export abstract class AbstractMarketplaceService {
  pkgs: MarketplacePkg[] = []
  data: MarketplaceData = {
    categories: [],
    name: '',
  }
  releaseNotes: {
    [id: string]: {
      [version: string]: string
    }
  } = {}
  marketplace: {
    url: string
    name: string
  } = { url: '', name: '' }

  abstract load(): Promise<void>

  abstract install(id: string, version?: string): Promise<void>

  abstract getPkg(id: string, version: string): Promise<MarketplacePkg>

  abstract cacheReleaseNotes(id: string): Promise<void>
}
