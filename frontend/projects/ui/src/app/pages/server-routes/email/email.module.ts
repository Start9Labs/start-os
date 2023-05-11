import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TuiButtonModule } from '@taiga-ui/core'
import { EmailPage } from './email.page'
import { Routes, RouterModule } from '@angular/router'
import { ReactiveFormsModule } from '@angular/forms'
import { FormModule } from 'src/app/components/form/form.module'

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
    ReactiveFormsModule,
    RouterModule.forChild(routes),
  ],
  declarations: [EmailPage],
})
export class EmailPageModule {}
