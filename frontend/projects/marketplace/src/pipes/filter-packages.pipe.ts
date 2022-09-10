import { NgModule, Pipe, PipeTransform } from '@angular/core'
import Fuse from 'fuse.js'

import { MarketplacePkg } from '../types/marketplace-pkg'
import { MarketplaceManifest } from '../types/marketplace-manifest'
import { Emver } from '@start9labs/shared'

@Pipe({
  name: 'filterPackages',
})
export class FilterPackagesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    packages: MarketplacePkg[],
    query: string,
    category: string,
    local: Record<string, { manifest: MarketplaceManifest }> = {},
  ): MarketplacePkg[] {
    // query
    if (query) {
      let options: Fuse.IFuseOptions<MarketplacePkg> = {
        includeScore: true,
        includeMatches: true,
      }

      if (query.length < 4) {
        options = {
          ...options,
          threshold: 0,
          location: 0,
          distance: 1,
          keys: [
            {
              name: 'manifest.title',
              weight: 1,
            },
            {
              name: 'manifest.id',
              weight: 0.5,
            },
          ],
        }
      } else {
        options = {
          ...options,
          ignoreLocation: true,
          useExtendedSearch: true,
          keys: [
            {
              name: 'manifest.title',
              weight: 1,
            },
            {
              name: 'manifest.id',
              weight: 0.5,
            },
            {
              name: 'manifest.description.short',
              weight: 0.4,
            },
            {
              name: 'manifest.description.long',
              weight: 0.1,
            },
          ],
        }
        query = `'${query}`
      }

      const fuse = new Fuse(packages, options)
      console.log(fuse.search(query))
      return fuse.search(query).map(p => p.item)
    }

    // updates
    if (category === 'updates') {
      return packages.filter(
        ({ manifest }) =>
          local[manifest.id] &&
          this.emver.compare(
            manifest.version,
            local[manifest.id].manifest.version,
          ) === 1,
      )
    }

    // category
    return packages
      .filter(p => category === 'all' || p.categories.includes(category))
      .sort((a, b) => {
        return a['published-at'] > b['published-at'] ? -1 : 1
      })
  }
}

@NgModule({
  declarations: [FilterPackagesPipe],
  exports: [FilterPackagesPipe],
})
export class FilterPackagesPipeModule {}
