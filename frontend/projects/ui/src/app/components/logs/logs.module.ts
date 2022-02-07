import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { LogsPage } from './logs.page'
import { TextSpinnerComponentModule } from '@start9labs/shared'

@NgModule({
  declarations: [LogsPage],
  imports: [CommonModule, IonicModule, TextSpinnerComponentModule],
  exports: [LogsPage],
})
export class LogsPageModule {}
