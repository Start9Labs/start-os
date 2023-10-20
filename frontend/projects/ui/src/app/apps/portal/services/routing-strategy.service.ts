import { inject, Injectable } from '@angular/core'
import {
  ActivatedRouteSnapshot,
  BaseRouteReuseStrategy,
  createUrlTreeFromSnapshot,
  DetachedRouteHandle,
  Route,
  UrlSerializer,
} from '@angular/router'
import { NavigationService } from './navigation.service'

@Injectable({
  providedIn: 'root',
})
export class RoutingStrategyService extends BaseRouteReuseStrategy {
  private readonly url = inject(UrlSerializer)
  private readonly navigation = inject(NavigationService)
  private readonly handlers = new Map<Route, DetachedRouteHandle>()

  override shouldDetach(route: ActivatedRouteSnapshot): boolean {
    const path = this.url.serialize(createUrlTreeFromSnapshot(route, ['.']))
    const inTabs = !!this.navigation.tabs.value.find(t => t.routerLink === path)

    if (!inTabs && route.routeConfig) {
      this.handlers.delete(route.routeConfig)
    }

    return path === '/portal/desktop' || inTabs
  }

  override store(
    route: ActivatedRouteSnapshot,
    handle: DetachedRouteHandle,
  ): void {
    if (route.routeConfig) this.handlers.set(route.routeConfig, handle)
  }

  override shouldAttach(route: ActivatedRouteSnapshot): boolean {
    return !!route.routeConfig && !!this.handlers.get(route.routeConfig)
  }

  override retrieve(route: ActivatedRouteSnapshot): DetachedRouteHandle | null {
    return (route.routeConfig && this.handlers.get(route.routeConfig)) || null
  }

  override shouldReuseRoute(
    future: ActivatedRouteSnapshot,
    curr: ActivatedRouteSnapshot,
  ): boolean {
    // return future.routeConfig === curr.routeConfig
    // TODO: Copied from ionic for backwards compatibility, remove later
    if (future.routeConfig !== curr.routeConfig) {
      return false
    }

    // checking router params
    const futureParams = future.params
    const currentParams = curr.params
    const keysA = Object.keys(futureParams)
    const keysB = Object.keys(currentParams)
    if (keysA.length !== keysB.length) {
      return false
    }
    // Test for A's keys different from B.
    for (const key of keysA) {
      if (currentParams[key] !== futureParams[key]) {
        return false
      }
    }
    return true
  }
}
