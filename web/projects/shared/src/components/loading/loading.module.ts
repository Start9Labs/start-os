import { NgModule } from '@angular/core'
import { TuiLoaderModule } from '@taiga-ui/core'
import { tuiAsDialog } from '@taiga-ui/cdk'
import { LoadingComponent } from './loading.component'
import { LoadingService } from './loading.service'

@NgModule({
  imports: [TuiLoaderModule],
  declarations: [LoadingComponent],
  exports: [LoadingComponent],
  providers: [tuiAsDialog(LoadingService)],
})
export class LoadingModule {}
