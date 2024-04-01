import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IntersectionObserverModule } from '@ng-web-apis/intersection-observer'
import { MutationObserverModule } from '@ng-web-apis/mutation-observer'
import { TuiLoaderModule, TuiScrollbarModule } from '@taiga-ui/core'
import { TuiBadgeModule, TuiButtonModule } from '@taiga-ui/experimental'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { LogsPage } from './logs.page'

const ROUTES: Routes = [
  {
    path: '',
    component: LogsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(ROUTES),
    IntersectionObserverModule,
    MutationObserverModule,
    NgDompurifyModule,
    TuiBadgeModule,
    TuiButtonModule,
    TuiLoaderModule,
    TuiScrollbarModule,
  ],
  declarations: [LogsPage],
})
export class LogsPageModule {}
