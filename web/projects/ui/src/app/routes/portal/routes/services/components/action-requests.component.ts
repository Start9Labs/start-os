import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceActionRequestComponent } from './action-request.component'

type ActionRequest = T.ActionRequest & {
  actionName: string
}

@Component({
  standalone: true,
  selector: 'service-action-requests',
  template: `
    @for (request of requests().critical; track $index) {
      <button [actionRequest]="request" [pkg]="pkg()">
        {{ request.actionName }}
        <small class="g-warning">Required</small>
      </button>
    }
    @for (request of requests().important; track $index) {
      <button [actionRequest]="request" [pkg]="pkg()">
        {{ request.actionName }}
        <small class="g-info">Requested</small>
      </button>
    }
    @if (requests().critical.length + requests().important.length === 0) {
      <blockquote>No pending tasks</blockquote>
    }
  `,
  styles: `
    small {
      margin-inline-start: 0.25rem;
      padding-inline-start: 0.5rem;
      box-shadow: inset 1px 0 var(--tui-border-normal);
    }

    blockquote {
      text-align: center;
      font: var(--tui-font-text-l);
      color: var(--tui-text-tertiary);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceActionRequestComponent],
})
export class ServiceActionRequestsComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly requests = computed(() => {
    const { id } = getManifest(this.pkg())
    const critical: ActionRequest[] = []
    const important: ActionRequest[] = []

    Object.values(this.pkg().requestedActions)
      .filter(r => r.active && r.request.packageId === id)
      .forEach(r => {
        const action = {
          ...r.request,
          actionName: this.pkg().actions[r.request.actionId].name,
        }

        if (r.request.severity === 'critical') {
          critical.push(action)
        } else {
          important.push(action)
        }
      })

    return { critical, important }
  })
}
