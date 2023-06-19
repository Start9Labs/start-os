import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { InsecureWarningComponentModule } from 'src/app/common/insecure-warning/insecure-warning.module'
import { BackupCreateDirective } from '../../directives/backup-create.directive'
import { BackupRestoreDirective } from '../../directives/backup-restore.directive'
import {
  BackingUpComponent,
  PkgMainStatusPipe,
} from '../../components/backing-up/backing-up.component'
import { BackupSelectPageModule } from '../../modals/backup-select/backup-select.module'
import { RecoverSelectPageModule } from '../../modals/recover-select/recover-select.module'
import { TargetPipesModule } from '../../pipes/target-pipes.module'
import { BackupsPage } from './backups.page'
import { PromptModule } from 'src/app/apps/ui/modals/prompt/prompt.module'

const routes: Routes = [
  {
    path: '',
    component: BackupsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BackupSelectPageModule,
    RecoverSelectPageModule,
    BadgeMenuComponentModule,
    InsecureWarningComponentModule,
    TargetPipesModule,
    PromptModule,
  ],
  declarations: [
    BackupsPage,
    BackupCreateDirective,
    BackupRestoreDirective,
    BackingUpComponent,
    PkgMainStatusPipe,
  ],
})
export class BackupsPageModule {}
