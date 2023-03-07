import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { LogsPage } from './logs.page'

const ROUTES: Routes = [
  {
    path: '',
    component: LogsPage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(ROUTES)],
  declarations: [LogsPage],
})
export class LogsPageModule {}
