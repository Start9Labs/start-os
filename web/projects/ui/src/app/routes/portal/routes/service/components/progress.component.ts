import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiProgressModule } from '@taiga-ui/kit'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/service/pipes/install-progress.pipe'

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
  @Input({ required: true }) progress!: T.Progress
}
