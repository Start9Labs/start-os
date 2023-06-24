import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiPromptModule } from '@taiga-ui/kit'
import { NotificationsPage } from './notifications.page'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { BackupReportPageModule } from '../../modals/backup-report/backup-report.module'

const routes: Routes = [
  {
    path: '',
    component: NotificationsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    SharedPipesModule,
    BackupReportPageModule,
    TuiPromptModule,
  ],
  declarations: [NotificationsPage],
})
export class NotificationsPageModule {}
