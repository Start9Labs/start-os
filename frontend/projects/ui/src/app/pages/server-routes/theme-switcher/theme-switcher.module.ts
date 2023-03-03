import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { TuiDialogModule } from '@taiga-ui/core'
import { TuiRadioListModule } from '@taiga-ui/kit'

import { ThemeSwitcherComponent } from './theme-switcher.component'

@NgModule({
  imports: [IonicModule, FormsModule, TuiDialogModule, TuiRadioListModule],
  declarations: [ThemeSwitcherComponent],
  exports: [ThemeSwitcherComponent],
})
export class ThemeSwitcherModule {}
