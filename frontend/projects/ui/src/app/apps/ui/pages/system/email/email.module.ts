import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TuiButtonModule, TuiNotificationModule } from '@taiga-ui/core'
import { EmailPage } from './email.page'
import { Routes, RouterModule } from '@angular/router'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { FormModule } from 'src/app/common/form/form.module'
import { TuiInputModule } from '@taiga-ui/kit'

const routes: Routes = [
  {
    path: '',
    component: EmailPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TuiButtonModule,
    FormModule,
    FormsModule,
    ReactiveFormsModule,
    TuiInputModule,
    TuiNotificationModule,
    RouterModule.forChild(routes),
  ],
  declarations: [EmailPage],
})
export class EmailPageModule {}
