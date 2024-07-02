// #flavor:0.1.2-beta.1:0

ExtendedVersion
  = flavor:Flavor? upstream:Version ":" downstream:Version {
    return { flavor: flavor || null, upstream, downstream }
  }

Flavor
  = "#" flavor:String ":" { return flavor }

String
  = [a-zA-Z]+ { return text(); }

Version
  = number:VersionNumber prerelease: PreRelease? {
    return { 
      number,
      prerelease: prerelease || []
    };
  }

PreRelease
  = "-" first:PreReleaseSegment rest:("." PreReleaseSegment)* {
    return [first].concat(rest.map(r => r[1]));
  }

PreReleaseSegment
  = "."? segment:(Digit / String) {
    return segment;
  }

VersionNumber
  = first:Digit rest:("." Digit)* { 
    return [first].concat(rest.map(r => r[1]));
  }

Digit
  = [0-9]+ { return parseInt(text(), 10); }

_ "whitespace"
  = [ \t\n\r]*