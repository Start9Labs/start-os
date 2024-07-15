import { TuiLet } from '@taiga-ui/cdk'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  Output,
} from '@angular/core'
import { TuiProgress } from '@taiga-ui/kit'
import { delay, filter } from 'rxjs'
import { LogsWindowComponent } from './logs-window.component'
import { SetupService } from '../../services/setup.service'

@Component({
  standalone: true,
  selector: 'app-initializing',
  template: `
    <section *tuiLet="progress$ | async as progress">
      <h1 [style.font-size.rem]="2.5" [style.margin.rem]="1">
        Initializing StartOS
      </h1>
      <div *ngIf="progress" class="center-wrapper">
        Progress: {{ (progress * 100).toFixed(0) }}%
      </div>

      <progress
        tuiProgressBar
        class="progress"
        [style.max-width.rem]="40"
        [style.margin]="'1rem auto'"
        [attr.value]="progress && progress < 1 ? progress : null"
      ></progress>
      <p>{{ getMessage(progress) }}</p>
    </section>
    <logs-window />
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
  readonly progress$ = inject(SetupService)

  @Input()
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'

  @Output()
  readonly finished = this.progress$.pipe(
    filter(progress => progress === 1),
    delay(500),
  )

  getMessage(progress: number | null): string {
    if (['fresh', 'attach'].includes(this.setupType || '')) {
      return 'Setting up your server'
    }

    if (!progress) {
      return 'Preparing data. This can take a while'
    } else if (progress < 1) {
      return 'Copying data'
    } else {
      return 'Finalizing'
    }
  }
}
