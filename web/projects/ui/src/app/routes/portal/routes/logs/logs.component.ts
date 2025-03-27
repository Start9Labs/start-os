import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import {
  TuiAppearance,
  TuiButton,
  TuiIcon,
  TuiLink,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCardMedium } from '@taiga-ui/layout'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'

interface Log {
  title: string
  subtitle: string
  icon: string
  follow: (params: RR.FollowServerLogsReq) => Promise<RR.FollowServerLogsRes>
  fetch: (params: RR.GetServerLogsReq) => Promise<RR.GetServerLogsRes>
}

@Component({
  template: `
    <ng-container *title>
      @if (current()) {
        <button
          tuiLink
          appearance=""
          iconStart="@tui.chevron-left"
          (click)="current.set(null)"
        >
          Back
        </button>
      } @else {
        Logs
      }
    </ng-container>
    @if (current(); as key) {
      <header tuiTitle>
        <strong>
          <button
            tuiIconButton
            appearance="secondary-grayscale"
            iconStart="@tui.x"
            size="s"
            (click)="current.set(null)"
          >
            Close
          </button>
          {{ logs[key].title }}
        </strong>
        <p tuiSubtitle>{{ logs[key].subtitle }}</p>
      </header>
      @for (log of logs | keyvalue; track $index) {
        @if (log.key === current()) {
          <logs
            [context]="log.value.title"
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
            <strong>{{ log.value.title }}</strong>
            <span tuiSubtitle>{{ log.value.subtitle }}</span>
          </span>
        </button>
      }
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  styles: [
    `
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

      logs {
        height: calc(100% - 4rem);
        width: 100%;
      }

      [tuiIconButton] {
        position: absolute;
        right: 0;
        border-radius: 100%;
      }

      button::before {
        margin: 0 -0.25rem 0 -0.375rem;
        --tui-icon-size: 1.5rem;
      }

      [tuiCardMedium] {
        cursor: pointer;
        box-shadow:
          inset 0 0 0 1px var(--tui-background-neutral-1),
          var(--tui-shadow-small);

        [tuiSubtitle] {
          color: var(--tui-text-secondary);
        }
      }

      :host-context(tui-root._mobile) {
        flex-direction: column;

        header {
          margin-top: -0.5rem;
          padding: 0;
        }

        logs {
          height: calc(100% - 3rem);
        }

        [tuiCardMedium] {
          width: 100%;
        }

        [tuiIconButton] {
          display: none;
        }
      }
    `,
  ],
  imports: [
    LogsComponent,
    TitleDirective,
    KeyValuePipe,
    TuiTitle,
    TuiCardMedium,
    TuiIcon,
    TuiAppearance,
    TuiLink,
    TuiButton,
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
