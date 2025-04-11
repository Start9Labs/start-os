import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { WaIntersectionObserver } from '@ng-web-apis/intersection-observer'
import { WaMutationObserver } from '@ng-web-apis/mutation-observer'
import { TuiButton, TuiLoader, TuiScrollbar } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
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
    ...WaIntersectionObserver,
    WaMutationObserver,
    NgDompurifyModule,
    TuiBadge,
    TuiButton,
    TuiLoader,
    TuiScrollbar,
  ],
  declarations: [LogsPage],
})
export class LogsPageModule {}
