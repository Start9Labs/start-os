import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { OSUpdatePage } from './os-update.page'
import { MarkdownPipeModule } from '@start9labs/shared'

@NgModule({
  declarations: [OSUpdatePage],
  imports: [CommonModule, IonicModule, MarkdownPipeModule],
  exports: [OSUpdatePage],
})
export class OSUpdatePageModule {}
