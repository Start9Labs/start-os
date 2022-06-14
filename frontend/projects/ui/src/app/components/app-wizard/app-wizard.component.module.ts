import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { AppWizardComponent } from './app-wizard.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { EmverPipesModule } from '@start9labs/shared'
import { DependentsComponentModule } from './dependents/dependents.component.module'
import { CompleteComponentModule } from './complete/complete.component.module'
import { NotesComponentModule } from './notes/notes.component.module'
import { AlertComponentModule } from './alert/alert.component.module'
import { SwiperModule } from 'swiper/angular'

@NgModule({
  declarations: [AppWizardComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    EmverPipesModule,
    DependentsComponentModule,
    CompleteComponentModule,
    NotesComponentModule,
    AlertComponentModule,
    SwiperModule,
  ],
  exports: [AppWizardComponent],
})
export class AppWizardComponentModule {}
