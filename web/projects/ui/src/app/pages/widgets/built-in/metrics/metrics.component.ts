import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'widget-metrics',
  templateUrl: './metrics.component.html',
  styleUrls: ['./metrics.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MetricsComponent {}
