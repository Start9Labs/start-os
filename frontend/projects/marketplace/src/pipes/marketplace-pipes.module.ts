import { NgModule } from '@angular/core'
import { InstallProgressPipe } from './install-progress.pipe'
import { TrustPipe } from './trust.pipe'
import { FilterPackagesPipe } from './filter-packages.pipe'

@NgModule({
  declarations: [InstallProgressPipe, TrustPipe, FilterPackagesPipe],
  exports: [InstallProgressPipe, TrustPipe, FilterPackagesPipe],
})
export class MarketplacePipesModule {}
