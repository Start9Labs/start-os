import { ChangeDetectionStrategy, Component, HostBinding } from '@angular/core'

@Component({
  selector: 'elastic-container',
  templateUrl: './elastic-container.component.html',
  styleUrls: ['./elastic-container.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ElasticContainerComponent {
  @HostBinding('style.height.px')
  height = NaN
}
