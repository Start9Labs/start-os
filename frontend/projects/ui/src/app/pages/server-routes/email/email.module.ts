import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TuiButtonModule } from '@taiga-ui/core'
import { EmailPage } from './email.page'
import { Routes, RouterModule } from '@angular/router'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { FormModule } from 'src/app/components/form/form.module'
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
    RouterModule.forChild(routes),
    TuiInputModule,
  ],
  declarations: [EmailPage],
})
export class EmailPageModule {}
