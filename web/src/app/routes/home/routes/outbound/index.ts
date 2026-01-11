import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet, Routes } from '@angular/router'
import { FormService } from 'src/app/services/form.service'
import { OutboundService } from './service'

@Component({
  template: '<router-outlet />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterOutlet],
  providers: [
    OutboundService,
    { provide: FormService, useExisting: OutboundService },
  ],
})
export class Outbound {}

export default [
  {
    path: '',
    component: Outbound,
    children: [
      {
        path: '',
        loadComponent: () => import('./routes/table'),
      },
      {
        path: ':label',
        loadComponent: () => import('./routes/vpn'),
      },
    ],
  },
] satisfies Routes
