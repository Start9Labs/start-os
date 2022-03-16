import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule, SharedPipesModule } from '@start9labs/shared'

import { PackageComponent } from './package.component'

@NgModule({
  imports: [IonicModule, SharedPipesModule, EmverPipesModule],
  declarations: [PackageComponent],
  exports: [PackageComponent],
})
export class PackageModule {}
