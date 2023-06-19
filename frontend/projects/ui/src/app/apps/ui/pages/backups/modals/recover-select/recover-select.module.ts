import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { TuiButtonModule, TuiGroupModule } from '@taiga-ui/core'
import { TuiCheckboxBlockModule } from '@taiga-ui/kit'
import { RecoverSelectPage } from './recover-select.page'
import { ToOptionsPipe } from './to-options.pipe'

@NgModule({
  declarations: [RecoverSelectPage, ToOptionsPipe],
  imports: [
    CommonModule,
    FormsModule,
    TuiButtonModule,
    TuiGroupModule,
    TuiCheckboxBlockModule,
  ],
  exports: [RecoverSelectPage],
})
export class RecoverSelectPageModule {}
