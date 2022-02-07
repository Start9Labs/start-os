import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { OSWelcomePage } from './os-welcome.page'
import { SharedPipesModule } from '@start9labs/shared'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [OSWelcomePage],
  imports: [CommonModule, IonicModule, FormsModule, SharedPipesModule],
  exports: [OSWelcomePage],
})
export class OSWelcomePageModule {}
