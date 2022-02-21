import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { DevManifestPage } from './dev-manifest.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { BackupReportPageModule } from 'src/app/modals/backup-report/backup-report.module'
import { FormsModule } from '@angular/forms'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'

const routes: Routes = [
  {
    path: '',
    component: DevManifestPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    BackupReportPageModule,
    FormsModule,
    MonacoEditorModule,
  ],
  declarations: [DevManifestPage],
})
export class DevManifestPageModule {}
