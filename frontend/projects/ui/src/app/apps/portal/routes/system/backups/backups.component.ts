import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiDialogService, TuiSvgModule } from '@taiga-ui/core'
import { BackupsCreateService } from './services/create.service'
import { BackupsRestoreService } from './services/restore.service'
import { BackupsUpcomingComponent } from './components/upcoming.component'
import { TARGETS } from './modals/targets.component'
import { HISTORY } from './modals/history.component'
import { JOBS } from './modals/jobs.component'

@Component({
  template: `
    <section>
      <h3 class="g-title">Options</h3>
      <button
        *ngFor="let option of options"
        class="g-action"
        (click)="option.action()"
      >
        <tui-svg [src]="option.icon"></tui-svg>
        <div>
          <strong>{{ option.name }}</strong>
          <div>{{ option.description }}</div>
        </div>
      </button>
    </section>
    <section>
      <h3 class="g-title">Upcoming Jobs</h3>
      <table backupsUpcoming class="g-table"></table>
    </section>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiSvgModule, BackupsUpcomingComponent],
})
export class BackupsComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly options = [
    {
      name: 'Create a Backup',
      icon: 'tuiIconPlusLarge',
      description: 'Create a one-time backup',
      action: inject(BackupsCreateService).handle,
    },
    {
      name: 'Restore from Backup',
      icon: 'tuiIconShareLarge',
      description: 'Restore services from a backup',
      action: inject(BackupsRestoreService).handle,
    },
    {
      name: 'Jobs',
      icon: 'tuiIconToolLarge',
      description: 'Manage backup jobs',
      action: () =>
        this.dialogs
          .open(JOBS, { label: 'Backup Jobs', size: 'l' })
          .subscribe(),
    },
    {
      name: 'Targets',
      icon: 'tuiIconDatabaseLarge',
      description: 'Manage backup targets',
      action: () =>
        this.dialogs.open(TARGETS, { label: 'Backup Targets' }).subscribe(),
    },
    {
      name: 'History',
      icon: 'tuiIconArchiveLarge',
      description: 'View your entire backup history',
      action: () =>
        this.dialogs
          .open(HISTORY, { label: 'Backup History', size: 'l' })
          .subscribe(),
    },
  ]
}
