import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { InstructionsPage } from './instructions.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { BackupReportPageModule } from 'src/app/modals/backup-report/backup-report.module'
import { FormsModule } from '@angular/forms'
import { CodeEditorModule } from '@ngstack/code-editor'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'

const routes: Routes = [
  {
    path: '',
    component: InstructionsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    SharingModule,
    BackupReportPageModule,
    FormsModule,
    CodeEditorModule.forRoot(),
    MonacoEditorModule,
  ],
  declarations: [InstructionsPage],
})
export class InstructionsPageModule {}
