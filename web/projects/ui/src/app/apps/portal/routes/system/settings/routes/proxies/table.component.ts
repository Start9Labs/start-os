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
import { TuiBadgeModule, TuiButtonModule } from '@taiga-ui/experimental'
import { Proxy } from 'src/app/services/patch-db/data-model'
import { ProxiesMenuComponent } from './menu.component'

@Component({
  selector: 'table[proxies]',
  template: `
    <thead>
      <tr>
        <th>Name</th>
        <th>Created</th>
        <th>Type</th>
        <th>Used By</th>
        <th></th>
      </tr>
    </thead>
    <tbody>
      <tr *ngFor="let proxy of proxies">
        <td>{{ proxy.name }}</td>
        <td>{{ proxy.createdAt | date: 'short' }}</td>
        <td>{{ proxy.type }}</td>
        <td>
          <button
            *ngIf="getLength(proxy); else unused"
            tuiLink
            (click)="onUsedBy(proxy)"
          >
            Connections: {{ getLength(proxy) }}
          </button>
          <ng-template #unused>N/A</ng-template>
        </td>
        <td><proxies-menu [proxy]="proxy" /></td>
      </tr>
    </tbody>
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButtonModule,
    TuiBadgeModule,
    TuiLinkModule,
    ProxiesMenuComponent,
  ],
})
export class ProxiesTableComponent {
  private readonly dialogs = inject(TuiDialogService)

  @Input()
  proxies: readonly Proxy[] = []

  @Output()
  readonly delete = new EventEmitter<Proxy>()

  getLength({ usedBy }: Proxy) {
    return usedBy.domains.length + usedBy.services.length
  }

  onUsedBy({ name, usedBy }: Proxy) {
    let message = `Proxy "${name}" is currently used by:`
    const domains = usedBy.domains.map(d => `<li>${d}</li>`)
    const services = usedBy.services.map(s => `<li>${s.title}</li>`)

    if (usedBy.domains.length) {
      message = `${message}<h2>Domains (inbound)</h2><ul>${domains}</ul>`
    }

    if (usedBy.services.length) {
      message = `${message}<h2>Services (outbound)</h2>${services}`
    }

    this.dialogs.open(message, { label: 'Used by', size: 's' }).subscribe()
  }
}
