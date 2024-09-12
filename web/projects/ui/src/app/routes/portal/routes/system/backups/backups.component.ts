import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiDialogService, TuiIcon } from '@taiga-ui/core'
import { BackupsUpcomingComponent } from './components/upcoming.component'
import { HISTORY } from './modals/history.component'
import { JOBS } from './modals/jobs.component'
import { TARGETS } from './modals/targets.component'
import { BackupsCreateService } from './services/create.service'
import { BackupsRestoreService } from './services/restore.service'

@Component({
  template: `
    <section>
      <h3 class="g-title">Options</h3>
      @for (option of options; track $index) {
        <button class="g-action" (click)="option.action()">
          <tui-icon [icon]="option.icon" />
          <div>
            <strong>{{ option.name }}</strong>
            <div>{{ option.description }}</div>
          </div>
        </button>
      }
    </section>
    <h3 class="g-title">Upcoming Jobs</h3>
    <table backupsUpcoming class="g-table"></table>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [BackupsUpcomingComponent, TuiIcon],
})
export default class BackupsComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly options = [
    {
      name: 'Create a Backup',
      icon: '@tui.plus',
      description: 'Create a one-time backup',
      action: inject(BackupsCreateService).handle,
    },
    {
      name: 'Restore from Backup',
      icon: '@tui.share',
      description: 'Restore services from a backup',
      action: inject(BackupsRestoreService).handle,
    },
    {
      name: 'Jobs',
      icon: '@tui.wrench',
      description: 'Manage backup jobs',
      action: () =>
        this.dialogs
          .open(JOBS, { label: 'Backup Jobs', size: 'l' })
          .subscribe(),
    },
    {
      name: 'Targets',
      icon: '@tui.database',
      description: 'Manage backup targets',
      action: () =>
        this.dialogs
          .open(TARGETS, { label: 'Backup Targets', size: 'l' })
          .subscribe(),
    },
    {
      name: 'History',
      icon: '@tui.archive',
      description: 'View your entire backup history',
      action: () =>
        this.dialogs
          .open(HISTORY, { label: 'Backup History', size: 'l' })
          .subscribe(),
    },
  ]
}