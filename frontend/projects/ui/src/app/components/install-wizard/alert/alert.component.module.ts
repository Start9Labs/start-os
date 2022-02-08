import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { AlertComponent } from './alert.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { MarkdownPipeModule } from '@start9labs/shared'

@NgModule({
  declarations: [AlertComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    MarkdownPipeModule,
  ],
  exports: [AlertComponent],
})
export class AlertComponentModule {}
