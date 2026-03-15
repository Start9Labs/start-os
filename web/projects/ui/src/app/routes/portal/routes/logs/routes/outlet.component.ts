import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TitleDirective } from 'src/app/services/title.service'

@Component({
  template: `
    <ng-container *title>{{ 'Logs' | i18n }}</ng-container>
    @for (log of logs; track $index) {
      <a [routerLink]="log.link">
        <tui-icon [icon]="log.icon" />
        <span tuiTitle>
          {{ log.title | i18n }}
          <span tuiSubtitle>{{ log.subtitle | i18n }}</span>
        </span>
        <tui-icon icon="@tui.chevron-right" />
      </a>
    }
  `,
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

      a {
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        padding: 0.75rem;
        border-radius: var(--tui-radius-l);
        background: var(--tui-background-neutral-1);
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

        [tuiCardMedium] {
          width: 100%;
          height: auto;
          gap: 1rem;
        }
      }
    `,
  ],
  imports: [RouterLink, TitleDirective, TuiTitle, TuiIcon, i18nPipe],
})
export default class SystemLogsComponent {
  readonly logs = [
    {
      link: 'os',
      title: 'OS Logs',
      subtitle: 'Raw, unfiltered operating system logs',
      icon: '@tui.square-dashed-bottom-code',
    },
    {
      link: 'kernel',
      title: 'Kernel Logs',
      subtitle: 'Diagnostics for drivers and other kernel processes',
      icon: '@tui.square-chevron-right',
    },
  ] as const
}
