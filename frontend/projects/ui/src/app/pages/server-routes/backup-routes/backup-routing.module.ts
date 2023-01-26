import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./backups/backups.module').then(m => m.BackupsPageModule),
  },
  {
    path: 'create',
    loadChildren: () =>
      import('./backup-create/backup-create.module').then(
        m => m.BackupCreatePageModule,
      ),
  },
  {
    path: 'restore',
    loadChildren: () =>
      import('./backup-restore/backup-restore.module').then(
        m => m.BackupRestorePageModule,
      ),
  },
  {
    path: 'jobs',
    loadChildren: () =>
      import('./backup-jobs/backup-jobs.module').then(
        m => m.BackupJobsPageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class BackupRoutingModule {}
