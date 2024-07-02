export type ValidExtendedVersion = {
  flavor: string | null,
  upstream: Version,
  downstream: Version,
}

export type Version = {
  number: number[],
  prerelease: PreReleaseSegment[],
}

export type PreReleaseSegment = number | string;
