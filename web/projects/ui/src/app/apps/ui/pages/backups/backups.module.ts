import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./pages/backups/backups.module').then(m => m.BackupsPageModule),
  },
  {
    path: 'jobs',
    loadChildren: () =>
      import('./pages/backup-jobs/backup-jobs.module').then(
        m => m.BackupJobsPageModule,
      ),
  },
  {
    path: 'targets',
    loadChildren: () =>
      import('./pages/backup-targets/backup-targets.module').then(
        m => m.BackupTargetsPageModule,
      ),
  },
  {
    path: 'history',
    loadChildren: () =>
      import('./pages/backup-history/backup-history.module').then(
        m => m.BackupHistoryPageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class BackupsModule {}
