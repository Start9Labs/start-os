import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { InstallWizardComponent } from './install-wizard.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { DependenciesComponentModule } from './dependencies/dependencies.component.module'
import { DependentsComponentModule } from './dependents/dependents.component.module'
import { CompleteComponentModule } from './complete/complete.component.module'
import { NotesComponentModule } from './notes/notes.component.module'

@NgModule({
  declarations: [
    InstallWizardComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    DependenciesComponentModule,
    DependentsComponentModule,
    CompleteComponentModule,
    NotesComponentModule,
  ],
  exports: [InstallWizardComponent],
})
export class InstallWizardComponentModule { }
