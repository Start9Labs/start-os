import { PatchDB } from 'patch-db-client'
import { NgModule } from '@angular/core'
import {
  BOOTSTRAPPER,
  PATCH_CACHE,
  PATCH_SOURCE,
  sourceFactory,
} from './patch-db.factory'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'

// This module is purely for providers organization purposes
@NgModule({
  providers: [
    {
      provide: BOOTSTRAPPER,
      useExisting: LocalStorageBootstrap,
    },
    {
      provide: PATCH_SOURCE,
      deps: [ApiService, AuthService, ConnectionService],
      useFactory: sourceFactory,
    },
    {
      provide: PatchDB,
      deps: [PATCH_SOURCE, PATCH_CACHE],
      useClass: PatchDB,
    },
  ],
})
export class PatchDbModule {}
