import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { EmailPage } from './email.page'
import { Routes, RouterModule } from '@angular/router'
import { FormsModule } from '@angular/forms'
import { FormObjectModule } from 'src/app/components/form-object/form-object.module'

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
    RouterModule.forChild(routes),
    FormsModule,
    FormObjectModule,
  ],
  declarations: [EmailPage],
})
export class EmailPageModule {}
