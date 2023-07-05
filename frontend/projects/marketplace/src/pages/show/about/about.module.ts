import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksModule,
} from '@start9labs/shared'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'

import { AboutComponent } from './about.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    MarkdownPipeModule,
    EmverPipesModule,
    NgDompurifyModule,
    SafeLinksModule,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
