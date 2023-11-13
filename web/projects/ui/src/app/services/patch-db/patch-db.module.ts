import { PatchDB } from 'patch-db-client'
import { Injector, NgModule } from '@angular/core'
import { PATCH_SOURCE, sourceFactory } from './patch-db.factory'

// This module is purely for providers organization purposes
@NgModule({
  providers: [
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
  ],
})
export class PatchDbModule {}
