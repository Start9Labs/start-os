import { Pipe, PipeTransform } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { MarketplacePkg } from '@start9labs/marketplace'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'filterUpdates',
})
export class FilterUpdatesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    pkgs: MarketplacePkg[],
    local: Record<string, PackageDataEntry | undefined>,
  ): MarketplacePkg[] {
    return pkgs.filter(
      ({ manifest }) =>
        this.emver.compare(
          manifest.version,
          local[manifest.id]?.manifest.version || '', // @TODO this won't work, need old version
        ) === 1,
    )
  }
}
