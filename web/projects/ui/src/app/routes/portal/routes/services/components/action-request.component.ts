import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { ActionService } from 'src/app/services/action.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'tr[actionRequest]',
  template: `
    <td>
      <tui-avatar size="xs"><img [src]="pkg().icon" alt="" /></tui-avatar>
      <span>{{ title() }}</span>
    </td>
    <td>
      @if (actionRequest().severity === 'critical') {
        <strong [style.color]="'var(--tui-status-warning)'">Required</strong>
      } @else {
        <strong [style.color]="'var(--tui-status-info)'">Optional</strong>
      }
    </td>
    <td
      [style.color]="'var(--tui-text-secondary)'"
      [style.grid-area]="'2 / span 2'"
    >
      {{ actionRequest().reason || 'No reason provided' }}
    </td>
    <td>
      <button tuiButton (click)="handle()">
        {{ pkg().actions[actionRequest().actionId].name }}
      </button>
    </td>
  `,
  styles: `
    td:first-child {
      white-space: nowrap;
      max-width: 10rem;
      overflow: hidden;
      text-overflow: ellipsis;
    }

    td:last-child {
      text-align: right;
      grid-area: span 2;
    }

    span {
      margin-inline-start: 0.5rem;
      vertical-align: middle;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: min-content 1fr min-content;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiAvatar],
})
export class ServiceActionRequestComponent {
  private readonly actionService = inject(ActionService)

  readonly actionRequest = input.required<T.ActionRequest>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly pkg = computed(() => this.services()[this.actionRequest().packageId])
  readonly title = computed(() => getManifest(this.pkg()).title)

  async handle() {
    this.actionService.present({
      pkgInfo: {
        id: this.actionRequest().packageId,
        title: this.title(),
        mainStatus: this.pkg().status.main,
        icon: this.pkg().icon,
      },
      actionInfo: {
        id: this.actionRequest().actionId,
        metadata: this.pkg().actions[this.actionRequest().actionId],
      },
      requestInfo: this.actionRequest(),
    })
  }
}
