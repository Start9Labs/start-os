import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupsPage } from './backups.page'

const routes: Routes = [
  {
    path: '',
    component: BackupsPage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(routes)],
  declarations: [BackupsPage],
})
export class BackupsPageModule {}
