import { PatchDB } from 'patch-db-client'
import { NgModule } from '@angular/core'
import { PATCH_SOURCE, sourceFactory } from './patch-db.factory'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { ResponseSyncService } from './response-sync.service'

// This module is purely for providers organization purposes
@NgModule({
  providers: [
    {
      provide: PATCH_SOURCE,
      deps: [ApiService, AuthService, ConnectionService, ResponseSyncService],
      useFactory: sourceFactory,
    },
    {
      provide: PatchDB,
      deps: [PATCH_SOURCE],
      useClass: PatchDB,
    },
  ],
})
export class PatchDbModule {}
