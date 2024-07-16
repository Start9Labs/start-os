import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ExverPipesModule, MarkdownPipeModule } from '@start9labs/shared'

import { AboutComponent } from './about.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    MarkdownPipeModule,
    ExverPipesModule,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
