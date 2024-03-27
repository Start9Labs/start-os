import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiProgressModule } from '@taiga-ui/kit'
import { Progress } from 'src/app/services/patch-db/data-model'
import { InstallingProgressPipe } from '../pipes/install-progress.pipe'

@Component({
  selector: '[progress]',
  template: `
    <ng-content></ng-content>
    @if (progress | installingProgress; as decimal) {
      : {{ decimal * 100 }}%
      <progress
        tuiProgressBar
        new
        size="xs"
        [style.color]="
          progress === true ? 'var(--tui-positive)' : 'var(--tui-link)'
        "
        [value]="decimal * 100"
      ></progress>
    }
  `,
  styles: [':host { line-height: 2rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiProgressModule, InstallingProgressPipe],
})
export class ServiceProgressComponent {
  @Input({ required: true }) progress!: Progress
}
