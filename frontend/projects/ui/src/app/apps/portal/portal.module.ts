import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { HeaderComponent } from './components/header/header.component'
import { PortalComponent } from './portal.component'
import { NavigationComponent } from './components/navigation/navigation.component'
import { DrawerComponent } from './components/drawer/drawer.component'

const ROUTES: Routes = [
  {
    path: '',
    component: PortalComponent,
    children: [
      {
        redirectTo: 'desktop',
        pathMatch: 'full',
        path: '',
      },
      {
        path: 'desktop',
        loadChildren: () =>
          import('./routes/desktop/desktop.module').then(m => m.DesktopModule),
      },
      {
        path: 'services',
        loadChildren: () =>
          import('./routes/services/services.module').then(
            m => m.ServicesModule,
          ),
      },
      {
        path: 'system',
        loadChildren: () =>
          import('./routes/system/system.module').then(m => m.SystemModule),
      },
    ],
  },
]

@NgModule({
  imports: [
    RouterModule.forChild(ROUTES),
    HeaderComponent,
    NavigationComponent,
    DrawerComponent,
  ],
  declarations: [PortalComponent],
  exports: [PortalComponent],
})
export class PortalModule {}
