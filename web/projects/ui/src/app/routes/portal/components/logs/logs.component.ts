import { CommonModule } from '@angular/common'
import { Component, ElementRef, Input, ViewChild } from '@angular/core'
import {
  WA_INTERSECTION_ROOT,
  WaIntersectionObserver,
} from '@ng-web-apis/intersection-observer'
import { WaMutationObserver } from '@ng-web-apis/mutation-observer'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiLoader, TuiScrollbar } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { BehaviorSubject } from 'rxjs'
import { FollowServerLogsReq } from 'src/app/services/api/api.types'
import { LogsDownloadDirective } from './logs-download.directive'
import { LogsFetchDirective } from './logs-fetch.directive'
import { LogsPipe } from './logs.pipe'

@Component({
  selector: 'logs',
  templateUrl: './logs.component.html',
  styleUrls: ['./logs.component.scss'],
  imports: [
    CommonModule,
    WaIntersectionObserver,
    WaMutationObserver,
    NgDompurifyPipe,
    TuiButton,
    TuiLoader,
    TuiScrollbar,
    LogsDownloadDirective,
    LogsFetchDirective,
    LogsPipe,
    i18nPipe,
  ],
  providers: [{ provide: WA_INTERSECTION_ROOT, useExisting: ElementRef }],
})
export class LogsComponent {
  @ViewChild('bottom')
  private readonly bottom?: ElementRef<HTMLElement>

  @ViewChild(TuiScrollbar, { read: ElementRef })
  private readonly scrollbar?: ElementRef<HTMLElement>

  @Input({ required: true }) followLogs!: (
    params: FollowServerLogsReq,
  ) => Promise<T.LogFollowResponse>

  @Input({ required: true }) fetchLogs!: (
    params: T.LogsParams,
  ) => Promise<T.LogResponse>

  @Input({ required: true }) context!: string

  scrollTop = 0
  startCursor?: string
  scroll = true
  loading = false
  previous: readonly string[] = []

  readonly status$ = new BehaviorSubject<
    'connected' | 'disconnected' | 'reconnecting'
  >('connected')

  onLoading(loading: boolean) {
    this.loading = loading && !this.scroll
  }

  onPrevious(previous: string) {
    this.onLoading(false)
    this.scrollTop = this.scrollbar?.nativeElement.scrollTop || 0
    this.previous = [previous, ...this.previous]
  }

  setCursor(startCursor = this.startCursor) {
    this.startCursor = startCursor
  }

  setScroll(scroll: boolean) {
    this.scroll = scroll
  }

  restoreScroll({ firstElementChild }: HTMLElement) {
    this.scrollbar?.nativeElement.scrollTo(
      this.scrollbar?.nativeElement.scrollLeft || 0,
      this.scrollTop + (firstElementChild?.clientHeight || 0),
    )
  }

  scrollToBottom() {
    if (this.scroll)
      this.bottom?.nativeElement.scrollIntoView({
        behavior: 'smooth',
      })
  }
}
