import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { TuiButtonModule, TuiGroupModule } from '@taiga-ui/core'
import { TuiCheckboxBlockModule } from '@taiga-ui/kit'
import { BackupSelectPage } from './backup-select.page'

@NgModule({
  declarations: [BackupSelectPage],
  imports: [
    CommonModule,
    FormsModule,
    TuiButtonModule,
    TuiGroupModule,
    TuiCheckboxBlockModule,
  ],
  exports: [BackupSelectPage],
})
export class BackupSelectPageModule {}
