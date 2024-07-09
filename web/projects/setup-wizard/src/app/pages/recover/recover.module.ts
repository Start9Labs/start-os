import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { RecoverPage } from './recover.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { RecoverPageRoutingModule } from './recover-routing.module'
import { CifsModalModule } from 'src/app/modals/cifs-modal/cifs-modal.module'

@NgModule({
  declarations: [RecoverPage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RecoverPageRoutingModule,
    PasswordPageModule,
    UnitConversionPipesModule,
    CifsModalModule,
  ],
})
export class RecoverPageModule {}
