import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ErrorMessageComponent } from './error-message.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    ErrorMessageComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [ErrorMessageComponent],
})
export class ErrorMessageComponentModule { }
