import { PackageState } from '@start9labs/shared'

import { MarketplaceManifest } from './marketplace-manifest'

export interface LocalPkg {
  state: PackageState
  manifest: MarketplaceManifest
}
