import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { TransferPage } from './transfer.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { TransferPageRoutingModule } from './transfer-routing.module'

@NgModule({
  declarations: [TransferPage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    TransferPageRoutingModule,
    PasswordPageModule,
    UnitConversionPipesModule,
  ],
})
export class TransferPageModule {}
