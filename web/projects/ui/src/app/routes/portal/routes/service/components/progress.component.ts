import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiProgress } from '@taiga-ui/kit'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/service/pipes/install-progress.pipe'

@Component({
  selector: '[progress]',
  template: `
    <ng-content />
    @if (progress | installingProgress; as percent) {
      : {{ percent }}%
      <progress
        tuiProgressBar
        size="xs"
        [style.color]="
          progress === true
            ? 'var(--tui-text-positive)'
            : 'var(--tui-text-action)'
        "
        [value]="percent / 100"
      ></progress>
    }
  `,
  styles: [':host { line-height: 2rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiProgress, InstallingProgressPipe],
})
export class ServiceProgressComponent {
  @Input({ required: true }) progress!: T.Progress
}
