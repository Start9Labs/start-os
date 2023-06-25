import { NgModule } from '@angular/core'
import { TuiButtonModule } from '@taiga-ui/core'
import { OSWelcomePage } from './os-welcome.page'

@NgModule({
  declarations: [OSWelcomePage],
  imports: [TuiButtonModule],
  exports: [OSWelcomePage],
})
export class OSWelcomePageModule {}
