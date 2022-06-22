import { InjectionToken } from '@angular/core'
import { exists } from '@start9labs/shared'
import { filter } from 'rxjs/operators'
import {
  Bootstrapper,
  DBCache,
  MockSource,
  PollSource,
  Source,
  WebsocketSource,
} from 'patch-db-client'

import { ConfigService } from '../config.service'
import { ApiService } from '../api/embassy-api.service'
import { MockApiService } from '../api/embassy-mock-api.service'
import { DataModel } from './data-model'
import { BehaviorSubject } from 'rxjs'

// [wsSources, pollSources]
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>[]>('')
export const PATCH_SOURCE$ = new InjectionToken<
  BehaviorSubject<Source<DataModel>[]>
>('')
export const PATCH_CACHE = new InjectionToken<DBCache<DataModel>>('', {
  factory: () => ({} as any),
})
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('')

export function mockSourceFactory({
  mockPatch$,
}: MockApiService): Source<DataModel>[] {
  return Array(2).fill(
    new MockSource<DataModel>(mockPatch$.pipe(filter(exists))),
  )
}

export function realSourceFactory(
  embassyApi: ApiService,
  config: ConfigService,
  { defaultView }: Document,
): Source<DataModel>[] {
  const { patchDb } = config
  const host = defaultView?.location.host
  const protocol = defaultView?.location.protocol === 'http:' ? 'ws' : 'wss'

  return [
    new WebsocketSource<DataModel>(`${protocol}://${host}/ws/db`),
    new PollSource<DataModel>({ ...patchDb.poll }, embassyApi),
  ]
}
