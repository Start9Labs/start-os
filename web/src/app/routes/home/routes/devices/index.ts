import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet, Routes } from '@angular/router'
import { provideFormService } from 'src/app/services/form.service'
import { DevicesService } from './service'

@Component({
  template: '<router-outlet />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterOutlet],
  providers: [provideFormService(DevicesService)],
})
export class Devices {}

export default [
  {
    path: '',
    component: Devices,
    children: [
      {
        path: '',
        loadComponent: () => import('./routes/table'),
      },
      {
        path: ':mac',
        loadComponent: () => import('./routes/device'),
      },
    ],
  },
] satisfies Routes
