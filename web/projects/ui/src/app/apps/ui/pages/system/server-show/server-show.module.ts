import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { ServerShowPage } from './server-show.page'
import { FormsModule } from '@angular/forms'
import { TextSpinnerComponentModule } from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { InsecureWarningComponentModule } from 'src/app/common/insecure-warning/insecure-warning.module'
import { OSUpdatePageModule } from './os-update/os-update.page.module'
import { PromptModule } from 'src/app/apps/ui/modals/prompt/prompt.module'
import { ThemeSwitcherModule } from '../theme-switcher/theme-switcher.module'
import { BackupColorPipe } from './backup-color.pipe'

const routes: Routes = [
  {
    path: '',
    component: ServerShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    TextSpinnerComponentModule,
    BadgeMenuComponentModule,
    OSUpdatePageModule,
    ThemeSwitcherModule,
    InsecureWarningComponentModule,
    PromptModule,
    RouterModule.forChild(routes),
  ],
  declarations: [ServerShowPage, BackupColorPipe],
})
export class ServerShowPageModule {}
