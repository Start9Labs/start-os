import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  BackupHistoryPage,
  DurationPipe,
  HasErrorPipe,
} from './backup-history.page'
import { TargetPipesModule } from '../../pipes/target-pipes.module'

const routes: Routes = [
  {
    path: '',
    component: BackupHistoryPage,
  },
]

@NgModule({
  declarations: [BackupHistoryPage, DurationPipe, HasErrorPipe],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    TargetPipesModule,
  ],
})
export class BackupHistoryPageModule {}
