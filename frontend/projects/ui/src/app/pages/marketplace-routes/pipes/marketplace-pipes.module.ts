import { NgModule } from '@angular/core'
import { InstallProgressPipe } from './install-progress.pipe'
import { TrustPipe } from './trust.pipe'

@NgModule({
  declarations: [InstallProgressPipe, TrustPipe],
  exports: [InstallProgressPipe, TrustPipe],
})
export class MarketplacePipesModule {}
