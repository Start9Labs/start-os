import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { LoadingPage, ToMessagePipe } from './loading.page'
import { LogsWindowComponent } from './logs-window/logs-window.component'
import { LoadingPageRoutingModule } from './loading-routing.module'

@NgModule({
  imports: [CommonModule, FormsModule, IonicModule, LoadingPageRoutingModule],
  declarations: [LoadingPage, ToMessagePipe, LogsWindowComponent],
})
export class LoadingPageModule {}
