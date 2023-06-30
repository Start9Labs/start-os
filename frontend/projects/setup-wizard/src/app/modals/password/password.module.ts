import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { TuiButtonModule, TuiErrorModule } from '@taiga-ui/core'
import { TuiInputPasswordModule } from '@taiga-ui/kit'
import { PasswordPage } from './password.page'

@NgModule({
  declarations: [PasswordPage],
  imports: [
    CommonModule,
    FormsModule,
    TuiButtonModule,
    TuiInputPasswordModule,
    TuiErrorModule,
    ReactiveFormsModule,
  ],
  exports: [PasswordPage],
})
export class PasswordPageModule {}
