import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { AboutComponent } from './about.component'
import { TuiTagModule } from '@taiga-ui/kit'

@NgModule({
  imports: [CommonModule, RouterModule, TuiTagModule],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
