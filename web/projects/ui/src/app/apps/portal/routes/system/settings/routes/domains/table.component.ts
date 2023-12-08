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
      <tr *ngFor="let domain of domains">
        <td>{{ domain.value }}</td>
        <td>{{ domain.createdAt | date : 'short' }}</td>
        <td>{{ domain.provider }}</td>
        <td>{{ getStrategy(domain) }}</td>
        <td>
          <button
            *ngIf="domain.usedBy.length as qty; else unused"
            tuiLink
            (click)="onUsedBy(domain)"
          >
            Interfaces: {{ qty }}
          </button>
          <ng-template #unused>N/A</ng-template>
        </td>
        <td>
          <button
            tuiIconButton
            size="xs"
            appearance="icon"
            iconLeft="tuiIconTrash2"
            [style.display]="'flex'"
            (click)="delete.emit(domain)"
          >
            Delete
          </button>
        </td>
      </tr>
    </tbody>
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
    return (
      domain.networkStrategy.ipStrategy ||
      domain.networkStrategy.proxyId ||
      'Primary Proxy'
    )
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
