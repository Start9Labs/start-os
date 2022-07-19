import { first } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { UIMarketplaceData } from 'src/app/services/patch-db/data-model'

export function getMarketplace(
  patch: PatchDbService,
): Promise<UIMarketplaceData> {
  return patch.watch$('ui', 'marketplace').pipe(first()).toPromise()
}
