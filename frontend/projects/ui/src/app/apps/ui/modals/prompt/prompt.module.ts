import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTextfieldControllerModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import { TuiAutoFocusModule } from '@taiga-ui/cdk'
import { PromptComponent } from './prompt.component'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    TuiInputModule,
    TuiButtonModule,
    TuiTextfieldControllerModule,
    TuiAutoFocusModule,
  ],
  declarations: [PromptComponent],
  exports: [PromptComponent],
})
export class PromptModule {}
