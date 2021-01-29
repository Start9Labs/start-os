import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { NotesComponent } from './notes.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    NotesComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [NotesComponent],
})
export class NotesComponentModule { }
