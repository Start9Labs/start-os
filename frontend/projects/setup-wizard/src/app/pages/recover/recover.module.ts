import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { DriveStatusComponent, RecoverPage } from './recover.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { ProdKeyModalModule } from '../../modals/prod-key-modal/prod-key-modal.module'
import { RecoverPageRoutingModule } from './recover-routing.module'
import { CifsModalModule } from 'src/app/modals/cifs-modal/cifs-modal.module'

@NgModule({
  declarations: [RecoverPage, DriveStatusComponent],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RecoverPageRoutingModule,
    PasswordPageModule,
    ProdKeyModalModule,
    UnitConversionPipesModule,
    CifsModalModule,
  ],
})
export class RecoverPageModule {}
