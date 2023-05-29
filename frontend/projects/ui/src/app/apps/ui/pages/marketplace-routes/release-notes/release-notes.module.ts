import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ReleaseNotesModule } from '@start9labs/marketplace'

import { ReleaseNotesPage } from './release-notes.page'

const routes: Routes = [
  {
    path: '',
    component: ReleaseNotesPage,
  },
]

@NgModule({
  imports: [IonicModule, ReleaseNotesModule, RouterModule.forChild(routes)],
  declarations: [ReleaseNotesPage],
  exports: [ReleaseNotesPage],
})
export class ReleaseNotesPageModule {}
