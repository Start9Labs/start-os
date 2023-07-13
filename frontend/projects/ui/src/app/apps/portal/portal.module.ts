import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { HeaderComponent } from './components/header/header.component'
import { PortalComponent } from './portal.component'
import { NavigationComponent } from './components/navigation/navigation.component'

const ROUTES: Routes = [
  {
    path: '',
    component: PortalComponent,
    children: [
      {
        redirectTo: 'services',
        pathMatch: 'full',
        path: '',
      },
      {
        path: 'services',
        loadChildren: () =>
          import('./routes/services/services.module').then(
            m => m.ServicesModule,
          ),
      },
    ],
  },
]

@NgModule({
  imports: [
    RouterModule.forChild(ROUTES),
    HeaderComponent,
    NavigationComponent,
  ],
  declarations: [PortalComponent],
  exports: [PortalComponent],
})
export class PortalModule {}
