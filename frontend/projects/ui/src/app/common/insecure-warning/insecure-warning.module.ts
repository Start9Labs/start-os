import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { InsecureWarningComponent } from './insecure-warning.component'

@NgModule({
  declarations: [InsecureWarningComponent],
  imports: [CommonModule, IonicModule],
  exports: [InsecureWarningComponent],
})
export class InsecureWarningComponentModule {}
