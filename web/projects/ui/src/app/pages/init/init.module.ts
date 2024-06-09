import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiProgressModule } from '@taiga-ui/kit'
import { LogsModule } from 'src/app/pages/init/logs/logs.module'
import { InitPage } from './init.page'

const routes: Routes = [
  {
    path: '',
    component: InitPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    LogsModule,
    TuiProgressModule,
    TuiLetModule,
    RouterModule.forChild(routes),
  ],
  declarations: [InitPage],
})
export class InitPageModule {}
