import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule } from '@angular/router'

import { AnyLinkComponent } from './any-link.component'

@NgModule({
  declarations: [AnyLinkComponent],
  imports: [CommonModule, RouterModule.forChild([])],
  exports: [AnyLinkComponent],
})
export class AnyLinkModule {}
