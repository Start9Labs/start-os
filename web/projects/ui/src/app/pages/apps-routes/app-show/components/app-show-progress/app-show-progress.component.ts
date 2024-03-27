import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { FullProgress } from '../../../../../../../../../../core/startos/bindings/FullProgress'

@Component({
  selector: 'app-show-progress',
  templateUrl: './app-show-progress.component.html',
  styleUrls: ['./app-show-progress.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowProgressComponent {
  @Input()
  phases!: FullProgress['phases']
}
