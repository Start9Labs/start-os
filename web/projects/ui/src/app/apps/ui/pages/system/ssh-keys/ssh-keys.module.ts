import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { GenericInputComponentModule } from 'src/app/apps/ui/modals/generic-input/generic-input.component.module'
import { SSHKeysPage } from './ssh-keys.page'

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
    GenericInputComponentModule,
    RouterModule.forChild(routes),
  ],
  declarations: [SSHKeysPage],
})
export class SSHKeysPageModule {}
