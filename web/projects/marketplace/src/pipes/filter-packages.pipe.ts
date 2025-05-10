import { NgModule, Pipe, PipeTransform } from '@angular/core'
import { MarketplacePkg } from '../types'
import Fuse from 'fuse.js'

@Pipe({
  name: 'filterPackages',
})
export class FilterPackagesPipe implements PipeTransform {
  transform(
    packages: MarketplacePkg[],
    query: string | null,
    category: string | null,
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
          threshold: 0.2,
          location: 0,
          distance: 16,
          keys: [
            {
              name: 'title',
              weight: 1,
            },
            {
              name: 'id',
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
              name: 'title',
              weight: 1,
            },
            {
              name: 'id',
              weight: 0.5,
            },
            {
              name: 'description.short',
              weight: 0.4,
            },
            {
              name: 'description.long',
              weight: 0.1,
            },
          ],
        }
        query = `'${query}`
      }

      const fuse = new Fuse(packages, options)
      return fuse.search(query).map(p => p.item)
    }

    // category
    return packages
      .filter(p => category === 'all' || p.categories.includes(category!))
      .sort((a, b) => {
        return (
          new Date(b.s9pk.publishedAt).valueOf() -
          new Date(a.s9pk.publishedAt).valueOf()
        )
      })
      .map(a => ({ ...a }))
  }
}

@NgModule({
  declarations: [FilterPackagesPipe],
  exports: [FilterPackagesPipe],
})
export class FilterPackagesPipeModule {}
