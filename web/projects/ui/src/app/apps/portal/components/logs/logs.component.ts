import { CommonModule } from '@angular/common'
import { Component, ElementRef, Input, ViewChild } from '@angular/core'
import { IntersectionObserverModule } from '@ng-web-apis/intersection-observer'
import { MutationObserverModule } from '@ng-web-apis/mutation-observer'
import { FetchLogsReq, FetchLogsRes } from '@start9labs/shared'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
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
    IntersectionObserverModule,
    MutationObserverModule,
    NgDompurifyModule,
    TuiButtonModule,
    TuiLoaderModule,
    LogsDownloadDirective,
    LogsFetchDirective,
    LogsPipe,
  ],
})
export class LogsComponent {
  @ViewChild('bottom')
  private readonly bottom?: ElementRef<HTMLElement>

  @Input({ required: true }) followLogs!: (
    params: RR.FollowServerLogsReq,
  ) => Promise<RR.FollowServerLogsRes>

  @Input({ required: true }) fetchLogs!: (
    params: FetchLogsReq,
  ) => Promise<FetchLogsRes>

  @Input({ required: true }) context!: string

  startCursor?: string
  scroll = true
  status$ = new BehaviorSubject<'connected' | 'disconnected' | 'reconnecting'>(
    'connected',
  )
  old: readonly string[] = []

  setCursor(startCursor = this.startCursor) {
    this.startCursor = startCursor
  }

  setScroll(scroll: boolean) {
    this.scroll = scroll
  }

  scrollToBottom() {
    if (this.scroll) this.bottom?.nativeElement.scrollIntoView()
  }
}
