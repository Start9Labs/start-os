import { ActivatedRouteSnapshot } from '@angular/router'

export function systemTabResolver({ data }: ActivatedRouteSnapshot): string {
  return data['title']
}
