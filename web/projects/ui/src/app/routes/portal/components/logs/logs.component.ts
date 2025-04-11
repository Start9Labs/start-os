import { CommonModule } from '@angular/common'
import { Component, ElementRef, Input, ViewChild } from '@angular/core'
import {
  INTERSECTION_ROOT,
  WaIntersectionObserver,
} from '@ng-web-apis/intersection-observer'
import { WaMutationObserver } from '@ng-web-apis/mutation-observer'
import { FetchLogsReq, FetchLogsRes } from '@start9labs/shared'
import { TuiLoader, TuiScrollbar, TuiButton } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { RR } from 'src/app/services/api/api.types'
import { LogsDownloadDirective } from './logs-download.directive'
import { LogsFetchDirective } from './logs-fetch.directive'
import { LogsPipe } from './logs.pipe'
import { BehaviorSubject } from 'rxjs'

@Component({
  standalone: true,
  selector: 'logs',
  templateUrl: './logs.component.html',
  styleUrls: ['./logs.component.scss'],
  imports: [
    CommonModule,
    WaIntersectionObserver,
    WaMutationObserver,
    NgDompurifyModule,
    TuiButton,
    TuiLoader,
    TuiScrollbar,
    LogsDownloadDirective,
    LogsFetchDirective,
    LogsPipe,
  ],
  providers: [
    {
      provide: INTERSECTION_ROOT,
      useExisting: ElementRef,
    },
  ],
})
export class LogsComponent {
  @ViewChild('bottom')
  private readonly bottom?: ElementRef<HTMLElement>

  @ViewChild(TuiScrollbar, { read: ElementRef })
  private readonly scrollbar?: ElementRef<HTMLElement>

  @Input({ required: true }) followLogs!: (
    params: RR.FollowServerLogsReq,
  ) => Promise<RR.FollowServerLogsRes>

  @Input({ required: true }) fetchLogs!: (
    params: FetchLogsReq,
  ) => Promise<FetchLogsRes>

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
