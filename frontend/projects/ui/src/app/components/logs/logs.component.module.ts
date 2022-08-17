import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { LogsComponent } from './logs.component'
import { FormsModule } from '@angular/forms'
import { TextSpinnerComponentModule } from '@start9labs/shared'

@NgModule({
  declarations: [LogsComponent],
  imports: [CommonModule, IonicModule, TextSpinnerComponentModule, FormsModule],
  exports: [LogsComponent],
})
export class LogsComponentModule {}
