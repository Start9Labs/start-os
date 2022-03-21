import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule, MarkdownPipeModule } from '@start9labs/shared'

import { AboutComponent } from './about.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    MarkdownPipeModule,
    EmverPipesModule,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
