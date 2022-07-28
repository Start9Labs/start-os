import { first } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { UIMarketplaceData } from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'

export function getMarketplace(
  patch: PatchDbService,
): Promise<UIMarketplaceData> {
  return firstValueFrom(patch.watch$('ui', 'marketplace'))
}
