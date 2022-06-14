import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { NotesComponent } from './notes.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { MarkdownPipeModule } from '@start9labs/shared'

@NgModule({
  declarations: [NotesComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    MarkdownPipeModule,
  ],
  exports: [NotesComponent],
})
export class NotesComponentModule {}
