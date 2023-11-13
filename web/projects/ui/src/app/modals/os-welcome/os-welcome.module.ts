import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { FormsModule } from '@angular/forms'
import { OSWelcomePage } from './os-welcome.page'

@NgModule({
  declarations: [OSWelcomePage],
  imports: [CommonModule, IonicModule, FormsModule, SharedPipesModule],
  exports: [OSWelcomePage],
})
export class OSWelcomePageModule {}
