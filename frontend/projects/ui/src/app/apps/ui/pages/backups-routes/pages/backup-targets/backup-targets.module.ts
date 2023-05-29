import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupTargetsPage } from './backup-targets.page'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'
import { UnitConversionPipesModule } from '@start9labs/shared'

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
    RouterModule.forChild(routes),
    SkeletonListComponentModule,
    UnitConversionPipesModule,
  ],
})
export class BackupTargetsPageModule {}
