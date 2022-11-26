import { NgModule, Pipe, PipeTransform } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { MarketplacePkg } from '../types'
import Fuse from 'fuse.js'

@Pipe({
  name: 'filterPackages',
})
export class FilterPackagesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    packages: MarketplacePkg[],
    query: string,
    category: string,
  ): MarketplacePkg[] {
    // query
    if (query) {
      let options: Fuse.IFuseOptions<MarketplacePkg> = {
        includeScore: true,
        includeMatches: true,
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

      const fuse = new Fuse(packages, options)
      return fuse.search(query).map(p => p.item)
    }

    // category
    return packages
      .filter(p => category === 'all' || p.categories.includes(category))
      .sort((a, b) => {
        return (
          new Date(b['published-at']).valueOf() -
          new Date(a['published-at']).valueOf()
        )
      })
  }
}

@NgModule({
  declarations: [FilterPackagesPipe],
  exports: [FilterPackagesPipe],
})
export class FilterPackagesPipeModule {}
