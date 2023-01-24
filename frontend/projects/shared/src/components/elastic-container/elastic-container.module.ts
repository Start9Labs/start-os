import { NgModule } from '@angular/core'
import { MutationObserverModule } from '@ng-web-apis/mutation-observer'

import { ElasticContainerComponent } from './elastic-container.component'
import { ElasticContainerDirective } from './elastic-container.directive'

@NgModule({
  imports: [MutationObserverModule],
  declarations: [ElasticContainerComponent, ElasticContainerDirective],
  exports: [ElasticContainerComponent],
})
export class ElasticContainerModule {}
