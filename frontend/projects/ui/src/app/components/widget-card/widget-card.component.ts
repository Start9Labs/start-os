import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  selector: 'widget-card',
  templateUrl: './widget-card.component.html',
  styleUrls: ['./widget-card.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WidgetCardComponent {
  @Input() title: string = ''
  @Input() icon: string = ''
  @Input() color: string = ''
  @Input() description: string = ''
  @Input() link: string = ''

  constructor() {}
}
