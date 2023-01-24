import { NgModule } from '@angular/core'

import { ElasticContainerComponent } from './elastic-container.component'
import { ElasticContainerDirective } from './elastic-container.directive'

@NgModule({
  declarations: [ElasticContainerComponent, ElasticContainerDirective],
  exports: [ElasticContainerComponent],
})
export class ElasticContainerModule {}
