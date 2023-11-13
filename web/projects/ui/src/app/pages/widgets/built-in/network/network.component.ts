import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'widget-network',
  templateUrl: './network.component.html',
  styleUrls: ['./network.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class NetworkComponent {}
