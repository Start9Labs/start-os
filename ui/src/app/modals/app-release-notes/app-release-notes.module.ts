import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppReleaseNotesPage } from './app-release-notes.page'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  declarations: [AppReleaseNotesPage],
})
export class AppReleaseNotesPageModule { }
