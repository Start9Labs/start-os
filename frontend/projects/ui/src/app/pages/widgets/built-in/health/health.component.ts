import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'widget-health',
  templateUrl: './health.component.html',
  styleUrls: ['./health.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HealthComponent {}
