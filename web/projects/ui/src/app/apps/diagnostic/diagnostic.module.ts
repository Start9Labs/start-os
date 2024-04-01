import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { WorkspaceConfig } from '@start9labs/shared'
import { DiagnosticService } from './services/diagnostic.service'
import { MockDiagnosticService } from './services/mock-diagnostic.service'
import { LiveDiagnosticService } from './services/live-diagnostic.service'

const { useMocks } = require('../../../../../../config.json') as WorkspaceConfig

const ROUTES: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./home/home.module').then(m => m.HomePageModule),
  },
  {
    path: 'logs',
    loadChildren: () =>
      import('./logs/logs.module').then(m => m.LogsPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
  providers: [
    {
      provide: DiagnosticService,
      useClass: useMocks ? MockDiagnosticService : LiveDiagnosticService,
    },
  ],
})
export class DiagnosticModule {}
