import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { DeveloperMenuPage } from './developer-menu.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { BackupReportPageModule } from 'src/app/modals/backup-report/backup-report.module'
import { GenericFormPageModule } from 'src/app/modals/generic-form/generic-form.module'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'
import { FormsModule } from '@angular/forms'
import { SharedPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: DeveloperMenuPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    BackupReportPageModule,
    GenericFormPageModule,
    FormsModule,
    MonacoEditorModule,
    SharedPipesModule,
  ],
  declarations: [DeveloperMenuPage],
})
export class DeveloperMenuPageModule {}
