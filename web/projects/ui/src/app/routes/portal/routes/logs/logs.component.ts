import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiAppearance, TuiButton, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCardMedium } from '@taiga-ui/layout'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'

interface Log {
  title: i18nKey
  subtitle: i18nKey
  icon: string
  follow: (params: RR.FollowServerLogsReq) => Promise<RR.FollowServerLogsRes>
  fetch: (params: RR.GetServerLogsReq) => Promise<RR.GetServerLogsRes>
}

@Component({
  template: `
    <ng-container *title>
      @if (current(); as key) {
        <button
          tuiIconButton
          iconStart="@tui.arrow-left"
          (click)="current.set(null)"
        >
          {{ 'Back' | i18n }}
        </button>
        {{ logs[key]?.title | i18n }}
      } @else {
        {{ 'Logs' | i18n }}
      }
    </ng-container>
    @if (current(); as key) {
      <header tuiTitle>
        <strong class="title">
          <button
            tuiIconButton
            appearance="secondary-grayscale"
            iconStart="@tui.x"
            size="s"
            class="close"
            (click)="current.set(null)"
          >
            {{ 'Close' | i18n }}
          </button>
          {{ logs[key]?.title | i18n }}
        </strong>
        <p tuiSubtitle>{{ logs[key]?.subtitle | i18n }}</p>
      </header>
      @for (log of logs | keyvalue; track $index) {
        @if (log.key === current()) {
          <logs
            [context]="log.key"
            [followLogs]="log.value.follow"
            [fetchLogs]="log.value.fetch"
          />
        }
      }
    } @else {
      @for (log of logs | keyvalue; track $index) {
        <button
          tuiCardMedium
          tuiAppearance="neutral"
          (click)="current.set(log.key)"
        >
          <tui-icon [icon]="log.value.icon" />
          <span tuiTitle>
            <strong>{{ log.value.title | i18n }}</strong>
            <span tuiSubtitle>{{ log.value.subtitle | i18n }}</span>
          </span>
          <tui-icon icon="@tui.chevron-right" />
        </button>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  styles: `
    :host {
      display: flex;
      align-items: center;
      justify-content: center;
      flex-wrap: wrap;
      gap: 1rem;
      padding: 1rem;
    }

    header {
      width: 100%;
      padding: 0 1rem;
    }

    strong {
      font-weight: 700;
    }

    logs {
      height: calc(100% - 4rem);
      width: 100%;
    }

    .close {
      position: absolute;
      right: 0;
      border-radius: 100%;
    }

    button::before {
      margin: 0 -0.25rem 0 -0.375rem;
      --tui-icon-size: 1.5rem;
    }

    [tuiCardMedium] {
      height: 14rem;
      width: 14rem;
      cursor: pointer;
      box-shadow:
        inset 0 0 0 1px var(--tui-background-neutral-1),
        var(--tui-shadow-small);

      [tuiSubtitle] {
        color: var(--tui-text-secondary);
      }

      tui-icon:last-child {
        align-self: flex-end;
      }
    }

    :host-context(tui-root._mobile) {
      flex-direction: column;
      justify-content: flex-start;

      header {
        padding: 0;
      }

      .title {
        display: none;
      }

      logs {
        height: calc(100% - 2rem);
      }

      [tuiCardMedium] {
        width: 100%;
        height: auto;
        gap: 1rem;
      }
    }
  `,
  imports: [
    LogsComponent,
    TitleDirective,
    KeyValuePipe,
    TuiTitle,
    TuiCardMedium,
    TuiIcon,
    TuiAppearance,
    TuiButton,
    i18nPipe,
  ],
})
export default class SystemLogsComponent {
  private readonly api = inject(ApiService)

  readonly current = signal<string | null>(null)
  readonly logs: Record<string, Log> = {
    os: {
      title: 'OS Logs',
      subtitle: 'Raw, unfiltered operating system logs',
      icon: '@tui.square-dashed-bottom-code',
      follow: params => this.api.followServerLogs(params),
      fetch: params => this.api.getServerLogs(params),
    },
    kernel: {
      title: 'Kernel Logs',
      subtitle: 'Diagnostics for drivers and other kernel processes',
      icon: '@tui.square-chevron-right',
      follow: params => this.api.followKernelLogs(params),
      fetch: params => this.api.getKernelLogs(params),
    },
    tor: {
      title: 'Tor Logs',
      subtitle: 'Diagnostic logs for the Tor daemon on StartOS',
      icon: '@tui.globe',
      follow: params => this.api.followTorLogs(params),
      fetch: params => this.api.getTorLogs(params),
    },
  }
}
