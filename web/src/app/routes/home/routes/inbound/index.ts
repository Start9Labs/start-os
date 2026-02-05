import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet, Routes } from '@angular/router'
import { provideFormService } from 'src/app/services/form.service'
import { InboundService } from './service'

@Component({
  template: '<router-outlet />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterOutlet],
  providers: [provideFormService(InboundService)],
})
class Inbound {}

export default [
  {
    path: '',
    component: Inbound,
    children: [
      {
        path: '',
        loadComponent: () => import('./routes/table'),
      },
      {
        path: ':listen_port',
        loadComponent: () => import('./routes/clients'),
      },
    ],
  },
] satisfies Routes
