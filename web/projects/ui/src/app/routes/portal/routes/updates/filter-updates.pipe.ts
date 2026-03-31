import { inject, Pipe, PipeTransform } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { MarketplacePkg } from '@start9labs/marketplace'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'filterUpdates',
})
export class FilterUpdatesPipe implements PipeTransform {
  private readonly exver = inject(Exver)

  transform(
    pkgs: MarketplacePkg[],
    local: Record<string, PackageDataEntry> = {},
  ): MarketplacePkg[] {
    return pkgs.filter(({ id, flavor, version }) => {
      const localPkg = local[id]
      return (
        !!localPkg &&
        (localPkg.stateInfo.state === 'installed' ||
          localPkg.stateInfo.state === 'updating') &&
        this.exver.getFlavor(localPkg.stateInfo.manifest.version) === flavor &&
        this.exver.compareExver(
          version,
          localPkg.stateInfo.manifest.version,
        ) === 1
      )
    })
  }
}
