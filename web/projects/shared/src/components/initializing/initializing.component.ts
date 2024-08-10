import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiProgress } from '@taiga-ui/kit'
import { LogsWindowComponent } from './logs-window.component'

@Component({
  standalone: true,
  selector: 'app-initializing',
  template: `
    <section>
      <h1 [style.font-size.rem]="2" [style.margin-bottom.rem]="2">
        Setting up your server
      </h1>
      <div *ngIf="progress.total">
        Progress: {{ (progress.total * 100).toFixed(0) }}%
      </div>
      <progress
        tuiProgressBar
        [style.max-width.rem]="40"
        [style.margin]="'1rem auto'"
        [attr.value]="progress.total"
      ></progress>
      <p>{{ progress.message }}</p>
    </section>
    <!--    <logs-window />-->
  `,
  styles: `
    section {
      border-radius: 0.25rem;
      padding: 1rem;
      margin: 1.5rem;
      text-align: center;
      /* TODO: Theme */
      background: #e0e0e0;
      color: #333;
      --tui-background-neutral-1: rgba(0, 0, 0, 0.1);
    }

    logs-window {
      display: flex;
      flex-direction: column;
      height: 18rem;
      padding: 1rem;
      margin: 0 1.5rem auto;
      text-align: left;
      overflow: hidden;
      border-radius: 2rem;
      /* TODO: Theme */
      background: #181818;
    }
  `,
  imports: [CommonModule, LogsWindowComponent, TuiLet, TuiProgress],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InitializingComponent {
  @Input()
  progress: { total: number; message: string } = { total: 0, message: '' }

  @Input()
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'
}
