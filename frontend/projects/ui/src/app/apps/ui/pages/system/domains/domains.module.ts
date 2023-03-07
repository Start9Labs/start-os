import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { DomainsPage } from './domains.page'
import { TuiNotificationModule } from '@taiga-ui/core'
import { SharedPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: DomainsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TuiNotificationModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
  ],
  declarations: [DomainsPage],
})
export class DomainsPageModule {}
