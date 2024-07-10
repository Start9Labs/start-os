import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  BackupDrivesComponent,
  BackupDrivesHeaderComponent,
  BackupDrivesStatusComponent,
} from './backup-drives.component'
import {
  UnitConversionPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

@NgModule({
  declarations: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
    BackupDrivesStatusComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    UnitConversionPipesModule,
    TextSpinnerComponentModule,
  ],
  exports: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
    BackupDrivesStatusComponent,
  ],
})
export class BackupDrivesComponentModule {}
