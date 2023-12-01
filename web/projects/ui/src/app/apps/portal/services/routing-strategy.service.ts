import { inject, Injectable } from '@angular/core'
import {
  ActivatedRouteSnapshot,
  BaseRouteReuseStrategy,
  createUrlTreeFromSnapshot,
  DetachedRouteHandle,
  UrlSerializer,
} from '@angular/router'
import { NavigationService } from './navigation.service'

@Injectable({
  providedIn: 'root',
})
export class RoutingStrategyService extends BaseRouteReuseStrategy {
  private readonly url = inject(UrlSerializer)
  private readonly navigation = inject(NavigationService)
  private readonly handlers = new Map<string, DetachedRouteHandle>()

  override shouldDetach(route: ActivatedRouteSnapshot): boolean {
    const path = this.getPath(route)
    const store = this.navigation.hasTab(path)

    if (!store) this.handlers.delete(path)

    return store && path.startsWith('/portal/service')
  }

  override store(
    route: ActivatedRouteSnapshot,
    handle: DetachedRouteHandle,
  ): void {
    this.handlers.set(this.getPath(route), handle)
  }

  override shouldAttach(route: ActivatedRouteSnapshot): boolean {
    return !!this.handlers.get(this.getPath(route))
  }

  override retrieve(route: ActivatedRouteSnapshot): DetachedRouteHandle | null {
    return this.handlers.get(this.getPath(route)) || null
  }

  override shouldReuseRoute(
    { routeConfig, params }: ActivatedRouteSnapshot,
    current: ActivatedRouteSnapshot,
  ): boolean {
    return (
      routeConfig === current.routeConfig &&
      Object.keys(params).length === Object.keys(current.params).length &&
      Object.keys(params).every(key => current.params[key] === params[key])
    )
  }

  private getPath(route: ActivatedRouteSnapshot): string {
    return this.url.serialize(createUrlTreeFromSnapshot(route, ['.']))
  }
}
