import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { MigratePage } from './migrate.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { MigratePageRoutingModule } from './migrate-routing.module'
import { CifsModalModule } from 'src/app/modals/cifs-modal/cifs-modal.module'

@NgModule({
  declarations: [MigratePage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    MigratePageRoutingModule,
    PasswordPageModule,
    UnitConversionPipesModule,
    CifsModalModule,
  ],
})
export class MigratePageModule {}
