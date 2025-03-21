import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IntersectionObserverModule } from '@ng-web-apis/intersection-observer'
import { MutationObserverModule } from '@ng-web-apis/mutation-observer'
import { TuiScrollbarModule } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { LogsComponent } from './logs.component'

@NgModule({
  imports: [
    CommonModule,
    MutationObserverModule,
    IntersectionObserverModule,
    NgDompurifyModule,
    TuiScrollbarModule,
  ],
  declarations: [LogsComponent],
  exports: [LogsComponent],
})
export class LogsModule {}
