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

export class ExtendedVersion {
  to_string(exver: ValidExtendedVersion): string {
    let exver_str = ""

    if (exver.flavor) {
      exver_str += '#';
      exver_str += exver.flavor;
      exver_str += ':';
    }

    exver_str = append_version(exver.upstream, exver_str);
    exver_str += ':';
    exver_str = append_version(exver.downstream, exver_str);

    return exver_str;
  }
}

function append_version(version: Version, str: string): string {
  version.number.forEach((n, i) => {
    str += n;
    if (i < version.number.length) {
      str += '.';
    }
  });

  if (version.prerelease.length > 0) {
    version.prerelease.forEach((n, i) => {
      if (typeof n === 'string') {
        str += '-';
      }
      str += n;
      if (i < version.prerelease.length) {
        str += '.';
      }
    })
  }

  return str;
}