import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { ActionService } from 'src/app/services/action.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'tr[actionRequest]',
  template: `
    <td tuiFade>
      <tui-avatar size="xs"><img [src]="pkg()?.icon" alt="" /></tui-avatar>
      <span>{{ title() }}</span>
    </td>
    <td>
      @if (actionRequest().severity === 'critical') {
        <strong [style.color]="'var(--tui-status-warning)'">
          {{ 'Required' | i18n }}
        </strong>
      } @else {
        <strong [style.color]="'var(--tui-status-info)'">
          {{ 'Optional' | i18n }}
        </strong>
      }
    </td>
    <td
      [style.color]="'var(--tui-text-secondary)'"
      [style.grid-area]="'2 / span 2'"
    >
      {{ actionRequest().reason || ('No reason provided' | i18n) }}
    </td>
    <td>
      <button tuiButton size="xs" (click)="handle()">
        {{ pkg()?.actions?.[actionRequest().actionId]?.name }}
      </button>
    </td>
  `,
  styles: `
    td:first-child {
      white-space: nowrap;
      max-width: 15rem;
      overflow: hidden;
    }

    td:last-child {
      text-align: right;
      grid-area: 3 / span 2;
    }

    span {
      margin-inline-start: 0.5rem;
      vertical-align: middle;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: 1fr min-content;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        display: flex;
        align-items: center;
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiAvatar, i18nPipe, TuiFade],
})
export class ServiceActionRequestComponent {
  private readonly actionService = inject(ActionService)

  readonly actionRequest = input.required<T.ActionRequest>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly pkg = computed(() => this.services()[this.actionRequest().packageId])
  readonly title = computed((pkg = this.pkg()) => pkg && getManifest(pkg).title)

  async handle() {
    const title = this.title()
    const pkg = this.pkg()
    const metadata = pkg?.actions[this.actionRequest().actionId]

    if (!title || !pkg || !metadata) {
      return
    }

    this.actionService.present({
      pkgInfo: {
        id: this.actionRequest().packageId,
        title,
        mainStatus: pkg.status.main,
        icon: pkg.icon,
      },
      actionInfo: {
        id: this.actionRequest().actionId,
        metadata,
      },
      requestInfo: this.actionRequest(),
    })
  }
}
