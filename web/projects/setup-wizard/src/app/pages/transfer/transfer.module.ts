import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  GuidPipePipesModule,
  UnitConversionPipesModule,
} from '@start9labs/shared'
import { TransferPage } from './transfer.page'
import { TransferPageRoutingModule } from './transfer-routing.module'

@NgModule({
  declarations: [TransferPage],
  imports: [
    CommonModule,
    IonicModule,
    TransferPageRoutingModule,
    UnitConversionPipesModule,
    GuidPipePipesModule,
  ],
})
export class TransferPageModule {}
