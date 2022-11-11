import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  SharedPipesModule,
  TickerModule,
} from '@start9labs/shared'

import { PackageComponent } from './package.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    EmverPipesModule,
    TickerModule,
  ],
  declarations: [PackageComponent],
  exports: [PackageComponent],
})
export class PackageModule {}
