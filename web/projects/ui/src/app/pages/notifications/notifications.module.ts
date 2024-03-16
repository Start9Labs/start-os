import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { NotificationsPage } from './notifications.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { SharedPipesModule } from '@start9labs/shared'
import { BackupReportPageModule } from 'src/app/modals/backup-report/backup-report.module'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'

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
    UiPipeModule,
  ],
  declarations: [NotificationsPage],
})
export class NotificationsPageModule {}
