// SPDX-FileCopyrightText: 2016 Michał Krasnoborski <mkrdln@gmail.com>
// SPDX-License-Identifier: MIT
#[macro_export]
macro_rules! cmd {
    ({$e:expr}) => ($e);

    // arg ToString splice
    ({$e:expr} (($a:expr)) $($tail:tt)*) =>
    {
        {
            // Allowing unused mut in case `$e` is of type `&mut Command`
            #[allow(unused_mut)]
            let mut cmd = $e;
            cmd.arg((&$a).to_string());
            cmd!( {cmd} $($tail)* )
        }
    };

    // arg splice
    ({$e:expr} ($a:expr) $($tail:tt)*) =>
    {
        {
            #[allow(unused_mut)]
            let mut cmd = $e;
            cmd.arg(&$a);
            cmd!( {cmd} $($tail)* )
        }
    };

    // args splice
    ({$e:expr} [$aa:expr] $($tail:tt)*) => {
        {
            #[allow(unused_mut)]
            let mut cmd = $e;
            cmd.args($aa);
            cmd!( {cmd} $($tail)* )
        }
    };

    // match
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*} ),* } $($tail:tt)*) => {
        cmd!({$e} match ($m) { $($($p)|+ $(if $g)* => {$($rr)*})* } $($tail)*)
    };
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*},)* } $($tail:tt)*) => {
        cmd!({$e} match ($m) { $($($p)|+ $(if $g)* => {$($rr)*})* } $($tail)*)
    };
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*} )* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {match $m { $($($p)|+ $(if $g)* => cmd!({cmd} $($rr)*)),* }} $($tail)* )
        }
    };

    // if let
    ({$e:expr} if let $p:pat = ($m:expr) { $($then:tt)* } else { $($els:tt)* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {
                    if let $p = $m { cmd!({cmd} $($then)*) } else { cmd!({cmd} $($els)*) }
                  } $($tail)*)
        }
    };
    ({$e:expr} if let $p:pat = ($m:expr) { $($then:tt)* } $($tail:tt)*) => {
        cmd!( {$e}if let $p = ($m) { $($then)* } else {} $($tail)* )
    };

    // if else
    ({$e:expr} if ($b:expr) { $($then:tt)* } else { $($els:tt)* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {
                    if $b { cmd!({cmd} $($then)*) } else { cmd!({cmd} $($els)*) }
                  } $($tail)*)
        }
    };
    ({$e:expr} if ($b:expr) { $($then:tt)* } $($tail:tt)*) => {
        cmd!( {$e}if ($b) { $($then)* } else {} $($tail)* )
    };

    // for
    ({$e:expr} for $p:pat in ($i:expr) { $($body:tt)* } $($tail:tt)*) => {
        {
            #[allow(unused_mut)]
            let mut cmd = $e;
            for $p in $i { cmd = cmd!( {cmd} $($body)* ); }
            cmd
        }
    };

    // naked ident
    ({$e:expr} $a:ident $($tail:tt)*) => (cmd!( {$e} (stringify!($a)) $($tail)* ));

    // Main entry points (command name)
    (($c:expr) $($tail:tt)*) => {
        cmd!( {::tokio::process::Command::new(&$c)} $($tail)* )
    };
    ($c:ident $($tail:tt)*) => (cmd!( (stringify!($c)) $($tail)* ));
}
