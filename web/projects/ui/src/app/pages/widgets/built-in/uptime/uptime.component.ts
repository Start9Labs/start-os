import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'widget-uptime',
  templateUrl: './uptime.component.html',
  styleUrls: ['./uptime.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class UptimeComponent {}
