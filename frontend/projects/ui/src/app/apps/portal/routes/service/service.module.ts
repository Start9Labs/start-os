import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { TuiLetModule } from '@taiga-ui/cdk'

import { ServiceComponent } from './service.component'
import { ServiceProgressComponent } from './components/progress.component'
import { ServiceStatusComponent } from './components/status.component'
import { ServiceActionsComponent } from './components/actions.component'
import { ServiceInterfaceComponent } from './components/interface.component'
import { ServiceHealthCheckComponent } from './components/health-check.component'
import { ServiceDependencyComponent } from './components/dependency.component'
import { ServiceMenuComponent } from './components/menu.component'
import { ServiceAdditionalComponent } from './components/additional.component'

import { ProgressDataPipe } from './pipes/progress-data.pipe'
import { ToDependenciesPipe } from './pipes/to-dependencies.pipe'
import { ToStatusPipe } from './pipes/to-status.pipe'
import { InterfaceInfoPipe } from './pipes/interface-info.pipe'
import { ToMenusPipe } from './pipes/to-menu.pipe'
import { ToAdditionalPipe } from './pipes/to-additional.pipe'

const ROUTES: Routes = [
  {
    path: ':pkgId',
    component: ServiceComponent,
  },
]

@NgModule({
  imports: [
    CommonModule,
    TuiLetModule,

    ServiceProgressComponent,
    ServiceStatusComponent,
    ServiceActionsComponent,
    ServiceInterfaceComponent,
    ServiceHealthCheckComponent,
    ServiceDependencyComponent,
    ServiceMenuComponent,
    ServiceAdditionalComponent,

    ProgressDataPipe,
    ToDependenciesPipe,
    ToStatusPipe,
    InterfaceInfoPipe,
    ToMenusPipe,
    ToAdditionalPipe,

    RouterModule.forChild(ROUTES),
  ],
  declarations: [ServiceComponent],
  exports: [ServiceComponent],
})
export class ServiceModule {}
