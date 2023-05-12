import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  SharedPipesModule,
  TickerModule,
} from '@start9labs/shared'

import { PackageComponent } from './package.component'
import { MimeTypePipeModule } from '../../../pipes/mime-type.pipe'

@NgModule({
  declarations: [PackageComponent],
  exports: [PackageComponent],
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    EmverPipesModule,
    TickerModule,
    MimeTypePipeModule,
  ],
})
export class PackageModule {}
