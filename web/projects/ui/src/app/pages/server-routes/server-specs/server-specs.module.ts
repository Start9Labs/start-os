import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ServerSpecsPage } from './server-specs.page'
import { EmverPipesModule } from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'

const routes: Routes = [
  {
    path: '',
    component: ServerSpecsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    QRComponentModule,
    EmverPipesModule,
    TuiLetModule,
  ],
  declarations: [ServerSpecsPage],
})
export class ServerSpecsPageModule {}
