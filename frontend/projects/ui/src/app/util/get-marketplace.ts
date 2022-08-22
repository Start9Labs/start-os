import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { UIMarketplaceData } from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom } from 'rxjs'

export function getMarketplace(
  patch: PatchDbService,
): Promise<UIMarketplaceData> {
  return firstValueFrom(patch.watch$('ui', 'marketplace').pipe(filter(Boolean)))
}
