import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { TuiButtonModule, TuiErrorModule } from '@taiga-ui/core'
import {
  TuiFieldErrorPipeModule,
  TuiInputModule,
  TuiInputPasswordModule,
} from '@taiga-ui/kit'
import { CifsModal } from './cifs-modal.page'

@NgModule({
  declarations: [CifsModal],
  imports: [
    CommonModule,
    FormsModule,
    TuiButtonModule,
    TuiInputModule,
    TuiErrorModule,
    ReactiveFormsModule,
    TuiFieldErrorPipeModule,
    TuiInputPasswordModule,
  ],
  exports: [CifsModal],
})
export class CifsModalModule {}
