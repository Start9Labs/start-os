import { inject, Pipe, PipeTransform } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { MarketplacePkg } from '@start9labs/marketplace'
import {
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'filterUpdates',
  standalone: true,
})
export class FilterUpdatesPipe implements PipeTransform {
  private readonly exver = inject(Exver)

  transform(
    pkgs: MarketplacePkg[],
    local: Record<
      string,
      PackageDataEntry<InstalledState | UpdatingState>
    > = {},
  ): MarketplacePkg[] {
    return pkgs.filter(({ id, version, flavor }) => {
      const localPkg = local[id]
      return (
        localPkg &&
        this.exver.getFlavor(localPkg.stateInfo.manifest.version) === flavor &&
        this.exver.compareExver(
          version,
          localPkg.stateInfo.manifest.version,
        ) === 1
      )
    })
  }
}
