import { InjectionToken } from '@angular/core'
import { exists } from '@start9labs/shared'
import { filter } from 'rxjs/operators'
import {
  Bootstrapper,
  DBCache,
  MockSource,
  Source,
  WebsocketSource,
} from 'patch-db-client'

import { MockApiService } from '../api/embassy-mock-api.service'
import { DataModel } from './data-model'

// [wsSources, pollSources]
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('')
export const PATCH_CACHE = new InjectionToken<DBCache<DataModel>>('', {
  factory: () => ({} as any),
})
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('')

export function mockSourceFactory({
  mockPatch$,
}: MockApiService): Source<DataModel> {
  return new MockSource<DataModel>(mockPatch$.pipe(filter(exists)))
}

export function realSourceFactory(
  _: any,
  { defaultView }: Document,
): Source<DataModel> {
  const host = defaultView?.location.host
  const protocol = defaultView?.location.protocol === 'http:' ? 'ws' : 'wss'

  return new WebsocketSource<DataModel>(`${protocol}://${host}/ws/db`)
}
