import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  TextSpinnerComponentModule,
  ElementModule,
} from '@start9labs/shared'
import { MarketplacePipesModule } from '@start9labs/marketplace'

import { ReleaseNotesPage } from './release-notes.page'
import { ReleaseNotesHeaderComponent } from './release-notes-header/release-notes-header.component'

const routes: Routes = [
  {
    path: '',
    component: ReleaseNotesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    EmverPipesModule,
    MarkdownPipeModule,
    MarketplacePipesModule,
    ElementModule,
  ],
  declarations: [ReleaseNotesPage, ReleaseNotesHeaderComponent],
  exports: [ReleaseNotesPage, ReleaseNotesHeaderComponent],
})
export class ReleaseNotesPageModule {}
