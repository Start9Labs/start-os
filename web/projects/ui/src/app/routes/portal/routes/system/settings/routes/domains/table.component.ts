import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import { TuiDialogService, TuiLinkModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { Domain } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'table[domains]',
  template: `
    <thead>
      <tr>
        <th>Domain</th>
        <th>Added</th>
        <th>DDNS Provider</th>
        <th>Network Strategy</th>
        <th>Used By</th>
        <th></th>
      </tr>
    </thead>
    <tbody>
      @for (domain of domains; track $index) {
        <tr *ngFor="let domain of domains">
          <td class="title">{{ domain.value }}</td>
          <td class="date">{{ domain.createdAt | date: 'medium' }}</td>
          <td class="provider">{{ domain.provider }}</td>
          <td class="strategy">{{ getStrategy(domain) }}</td>
          <td class="used">
            @if (domain.usedBy.length; as qty) {
              <button tuiLink (click)="onUsedBy(domain)">
                Interfaces: {{ qty }}
              </button>
            } @else {
              N/A
            }
          </td>
          <td class="actions">
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconLeft="tuiIconTrash2"
              (click)="delete.emit(domain)"
            >
              Delete
            </button>
          </td>
        </tr>
      } @empty {
        <tr><td colspan="6">No domains</td></tr>
      }
    </tbody>
  `,
  styles: `
    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr 1fr;
      }

      td:only-child {
        grid-column: span 2;
      }

      .title {
        order: 1;
        font-weight: bold;
      }

      .actions {
        order: 2;
        padding: 0;
        text-align: right;
      }

      .provider {
        order: 3;
      }

      .strategy {
        order: 4;
        text-align: right;
      }

      .date {
        order: 5;
        color: var(--tui-text-03);
      }

      .used {
        order: 6;
        text-align: right;

        &:not(:has(button)) {
          display: none;
        }
      }
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiButtonModule, TuiLinkModule],
})
export class DomainsTableComponent {
  private readonly dialogs = inject(TuiDialogService)

  @Input()
  domains: readonly Domain[] = []

  @Output()
  readonly delete = new EventEmitter<Domain>()

  getStrategy(domain: any) {
    return domain.networkStrategy.ipStrategy || domain.networkStrategy.proxy
  }

  onUsedBy({ value, usedBy }: Domain) {
    const interfaces = usedBy.map(u =>
      u.interfaces.map(i => `<li>${u.service.title} - ${i.title}</li>`),
    )

    this.dialogs
      .open(`${value} is currently being used by:<ul>${interfaces}</ul>`, {
        label: 'Used by',
        size: 's',
      })
      .subscribe()
  }
}
