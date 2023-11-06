import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { MarkdownPipeModule, SafeLinksDirective } from '@start9labs/shared'
import { TuiScrollbarModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiAutoFocusModule } from '@taiga-ui/cdk'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { OSUpdatePage } from './os-update.page'

@NgModule({
  declarations: [OSUpdatePage],
  imports: [
    CommonModule,
    MarkdownPipeModule,
    TuiButtonModule,
    TuiAutoFocusModule,
    TuiScrollbarModule,
    SafeLinksDirective,
    NgDompurifyModule,
  ],
  exports: [OSUpdatePage],
})
export class OSUpdatePageModule {}
