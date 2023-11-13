import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { HealthCheckResult } from 'src/app/services/patch-db/data-model'
import { ConnectionService } from 'src/app/services/connection.service'
import { ServiceHealthCheckComponent } from './health-check.component'

@Component({
  selector: 'service-health-checks',
  template: `
    <h3 class="g-title">Health Checks</h3>
    <service-health-check
      *ngFor="let check of checks"
      class="g-action"
      [check]="check"
      [connected]="!!(connected$ | async)"
    ></service-health-check>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, ServiceHealthCheckComponent],
})
export class ServiceHealthChecksComponent {
  @Input({ required: true })
  checks: readonly HealthCheckResult[] = []

  readonly connected$ = inject(ConnectionService).connected$
}
