import { PatchDB } from 'patch-db-client'
import { Injector } from '@angular/core'
import { PATCH_SOURCE, sourceFactory } from './patch-db.factory'

export const PATCH_DB_PROVIDERS = [
  {
    provide: PATCH_SOURCE,
    deps: [Injector],
    useFactory: sourceFactory,
  },
  {
    provide: PatchDB,
    deps: [PATCH_SOURCE],
    useClass: PatchDB,
  },
]
