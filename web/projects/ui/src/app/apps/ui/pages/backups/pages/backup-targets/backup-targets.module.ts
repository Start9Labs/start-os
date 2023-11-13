import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { SkeletonListComponentModule } from 'src/app/common/skeleton-list/skeleton-list.component.module'
import { FormPageModule } from 'src/app/apps/ui/modals/form/form.module'
import { BackupTargetsPage } from './backup-targets.page'
import { TuiNotificationModule } from '@taiga-ui/core'

const routes: Routes = [
  {
    path: '',
    component: BackupTargetsPage,
  },
]

@NgModule({
  declarations: [BackupTargetsPage],
  imports: [
    CommonModule,
    IonicModule,
    SkeletonListComponentModule,
    UnitConversionPipesModule,
    FormPageModule,
    RouterModule.forChild(routes),
    TuiNotificationModule,
  ],
})
export class BackupTargetsPageModule {}
