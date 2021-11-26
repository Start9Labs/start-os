import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppShowPage, HealthColorPipe } from './app-show.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
import { AppConfigPageModule } from 'src/app/modals/app-config/app-config.module'
import { MarkdownPageModule } from 'src/app/modals/markdown/markdown.module'

const routes: Routes = [
  {
    path: '',
    component: AppShowPage,
  },
]

@NgModule({
  declarations: [
    AppShowPage,
    HealthColorPipe,
  ],
  imports: [
    CommonModule,
    StatusComponentModule,
    IonicModule,
    RouterModule.forChild(routes),
    InstallWizardComponentModule,
    AppConfigPageModule,
    SharingModule,
    MarkdownPageModule,
  ],
})
export class AppShowPageModule { }

