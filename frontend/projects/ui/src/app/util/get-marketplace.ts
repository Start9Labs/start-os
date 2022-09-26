import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  UIMarketplaceData,
} from 'src/app/services/patch-db/data-model'
import { firstValueFrom, map } from 'rxjs'

export function getMarketplace(
  patch: PatchDB<DataModel>,
): Promise<UIMarketplaceData> {
  return firstValueFrom(
    patch.watch$('ui', 'marketplace').pipe(
      map(
        m =>
          m || {
            'selected-id': null,
            'known-hosts': {},
          },
      ),
    ),
  )
}
