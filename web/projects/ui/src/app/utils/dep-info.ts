import {
  AllPackageData,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

export function getDepDetails(
  pkg: PackageDataEntry,
  allPkgs: AllPackageData,
  depId: string,
) {
  const { title, icon, versionRange } = pkg.currentDependencies[depId] || {}

  if (
    allPkgs[depId] &&
    (allPkgs[depId].stateInfo.state === 'installed' ||
      allPkgs[depId].stateInfo.state === 'updating')
  ) {
    return {
      title: allPkgs[depId].stateInfo.manifest!.title,
      icon: allPkgs[depId].icon,
      versionRange,
    }
  } else {
    return {
      title: title || depId,
      icon: icon || 'assets/img/service-icons/fallback.png',
      versionRange,
    }
  }
}
