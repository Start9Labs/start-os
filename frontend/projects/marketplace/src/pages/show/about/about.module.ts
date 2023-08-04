import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksModule,
  SharedPipesModule,
} from '@start9labs/shared'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'

import { AboutComponent } from './about.component'
import { DependenciesModule } from '../dependencies/dependencies.module'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    MarkdownPipeModule,
    EmverPipesModule,
    NgDompurifyModule,
    SafeLinksModule,
    DependenciesModule,
    SharedPipesModule,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
