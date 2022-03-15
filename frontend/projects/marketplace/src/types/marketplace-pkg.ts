import { Url } from '@start9labs/shared'
import { MarketplaceManifest } from './marketplace-manifest'

export interface MarketplacePkg {
  icon: Url
  license: Url
  instructions: Url
  manifest: MarketplaceManifest
  categories: string[]
  versions: string[]
  'dependency-metadata': {
    [id: string]: {
      title: string
      icon: Url
    }
  }
}
