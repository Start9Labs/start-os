import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiProgressModule } from '@taiga-ui/kit'

@Component({
  selector: '[progress]',
  template: `
    <ng-content></ng-content>
    : {{ progress }}%
    <progress
      tuiProgressBar
      new
      size="xs"
      [style.color]="
        progress === 100 ? 'var(--tui-positive)' : 'var(--tui-link)'
      "
      [value]="progress / 100"
    ></progress>
  `,
  styles: [':host { line-height: 2rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiProgressModule],
})
export class ServiceProgressComponent {
  @Input({ required: true })
  progress = 0
}
