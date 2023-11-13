import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { PromptModule } from 'src/app/apps/ui/modals/prompt/prompt.module'
import { SSHKeysPage } from './ssh-keys.page'
import { TuiNotificationModule } from '@taiga-ui/core'

const routes: Routes = [
  {
    path: '',
    component: SSHKeysPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    PromptModule,
    TuiNotificationModule,
    RouterModule.forChild(routes),
  ],
  declarations: [SSHKeysPage],
})
export class SSHKeysPageModule {}
