import { Component, Input } from '@angular/core'
import { TuiRepeatTimesModule } from '@taiga-ui/cdk'

@Component({
  selector: 'skeleton-list',
  template: `
    <div *tuiRepeatTimes="let index of rows" class="g-action">
      <div
        class="tui-skeleton"
        style="--tui-skeleton-radius: 100%; width: 2.5rem; height: 2.5rem"
        [hidden]="!showAvatar"
      ></div>
      <div class="tui-skeleton" style="width: 12rem; height: 0.75rem"></div>
      <div
        class="tui-skeleton"
        style="width: 5rem; height: 0.75rem; margin-left: auto"
      ></div>
    </div>
  `,
  standalone: true,
  imports: [TuiRepeatTimesModule],
})
export class SkeletonListComponent {
  @Input() rows = 3
  @Input() showAvatar = false
}
