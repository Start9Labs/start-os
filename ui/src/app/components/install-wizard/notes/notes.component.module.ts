import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { DeveloperNotesComponent } from './notes.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    DeveloperNotesComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [DeveloperNotesComponent],
})
export class DeveloperNotesComponentModule { }
