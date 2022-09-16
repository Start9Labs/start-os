import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  UIMarketplaceData,
} from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom, startWith } from 'rxjs'

export function getMarketplace(
  patch: PatchDB<DataModel>,
): Promise<UIMarketplaceData> {
  return firstValueFrom(
    patch.watch$('ui', 'marketplace').pipe(
      filter(Boolean),
      startWith({
        'selected-id': null,
        'known-hosts': {},
      }),
    ),
  )
}
