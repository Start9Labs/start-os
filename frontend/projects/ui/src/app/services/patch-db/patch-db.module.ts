import { PatchDB } from 'patch-db-client'
import { NgModule } from '@angular/core'
import { DOCUMENT } from '@angular/common'
import { WorkspaceConfig } from '@start9labs/shared'

import {
  BOOTSTRAPPER,
  mockSourceFactory,
  PATCH_CACHE,
  PATCH_SOURCE,
  PATCH_SOURCE$,
  realSourceFactory,
} from './patch-db.factory'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { ApiService } from '../api/embassy-api.service'
import { ConfigService } from '../config.service'
import { ReplaySubject } from 'rxjs'

const { useMocks } = require('../../../../../../config.json') as WorkspaceConfig

// This module is purely for providers organization purposes
@NgModule({
  providers: [
    {
      provide: BOOTSTRAPPER,
      useExisting: LocalStorageBootstrap,
    },
    {
      provide: PATCH_SOURCE,
      deps: [ApiService, ConfigService, DOCUMENT],
      useFactory: useMocks ? mockSourceFactory : realSourceFactory,
    },
    {
      provide: PATCH_SOURCE$,
      useValue: new ReplaySubject(1),
    },
    {
      provide: PatchDB,
      deps: [PATCH_SOURCE$, ApiService, PATCH_CACHE],
      useClass: PatchDB,
    },
  ],
})
export class PatchDbModule {}
