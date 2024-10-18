import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import {
  ExverPipesModule,
  SharedPipesModule,
  TickerModule,
} from '@start9labs/shared'

import { PackageComponent } from './package.component'

@NgModule({
  declarations: [PackageComponent],
  exports: [PackageComponent],
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    ExverPipesModule,
    TickerModule,
  ],
})
export class PackageModule {}
