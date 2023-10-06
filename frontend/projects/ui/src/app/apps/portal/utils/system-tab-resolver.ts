import { ActivatedRouteSnapshot } from '@angular/router'
import { inject } from '@angular/core'
import { NavigationService } from '../services/navigation.service'
import { NavigationItem } from '../types/navigation-item'

export function systemTabResolver({ data }: ActivatedRouteSnapshot): string {
  inject(NavigationService).addTab(data as NavigationItem)

  return data['title']
}
