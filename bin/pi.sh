#!/bin/bash

function pi() {
    local srcs args
    while [ $# -gt 0 ]
    do
        if [ "$1" = '--' ]
        then
            break
        fi
        srcs="$srcs $1"
        shift
    done
    while [ $# -gt 0 ]
    do
        args="$args $1"
        shift
    done
    set ../pi.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.5.0-RC1 \
                  --dep org.typelevel::cats-effect:3.6-0142603 \
                  ${args#?} \
                  2>&1
}

function pi_() {
    local srcs args
    while [ $# -gt 0 ]
    do
        if [ "$1" = '--' ]
        then
            break
        fi
        srcs="$srcs $1"
        shift
    done
    while [ $# -gt 0 ]
    do
        args="$args $1"
        shift
    done
    set ../pi_.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.5.0-RC1 \
                  --dep org.typelevel::cats-effect:3.6-0142603 \
                  ${args#?} \
                  2>&1
}

function pio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --non-interactive --stdin >| "$1".scala
        shift
    done
}

function dotarrowCount() {
    expr $(cat "$1" | tr '\\' ' ' | awk '{ print $1 }' | sed -e 's/$/ + /' | tr -d '\n'; echo 0)
}

function dotarrowCountPreLast() {
    if [ `cat "$1" | tail -n 2 | wc -l` -eq 2 ]
    then
        expr $(cat "$1" | tail -n 2 | head -n 1 | tr '\\' ' ' | awk '{ print $1 }' | sed -e 's/$/ + /' | tr -d '\n'; echo 0)
    else
        echo 0
    fi
}

function dotarrowCountLast() {
    expr $(cat "$1" | tail -n 1 | tr '\\' ' ' | awk '{ print $1 }' | sed -e 's/$/ + /' | tr -d '\n'; echo 0)
}

function dotarrowScalaToHaskell0() {
    local x=dotarrow
    local p="`readlink -m \"$x/tmp/$1\"`"
    local _24="`printf '%24s'`"
    local _24="${_24// /\\ }"
    local n=`dotarrowCount "$p.txt"`
    let j=`dotarrowCountLast "$p.txt"`
    let n=n-j
    let k=`dotarrowCountPreLast "$p.txt"`
    let m=n-k
    [ $m -ge 0 ] || let m=0
    sed -i "$p/app/Inp_"*.hs -e "10,+${m}d"
    sed -i "$p/app/Out_"*.hs -e "11,+$((m+k))d"
    cat "$p.txt" | head -n -1 |
    while read count_name_tpe
    do
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            local c=,
        done
    done
    cat "$p.txt" | tail -n 1 |
    if read count_name_tpe
    then
        [ $n -eq 0 ] || local c=,
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            local c=,
        done
    fi
    [ $n -eq 0 ] || local c=,
    sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
    sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24},\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
}

function dotarrowScalaToHaskell() {
    [ $# -eq 2 ] || return
    local x=dotarrow
    local app="../$x/sc2hs"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local q="`readlink -m \"$x/tmp/$2\"`"
    scala-cli run -S 3.5.0-RC1 "../$x/source.scala" -- "$x/$2.scala"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/sc2hs" &>/dev/null; cp -r "$app" "$p/tmp"
    local hs="`cat \"$x/src/$2.scala.src\" | Scalameta2Haskell.pl`"
    pushd "$p/tmp/sc2hs" &>/dev/null
    mv app/Main.hs{.in,}
    sed -i app/Main.hs \
        -e '/^\s*source\s*=\s*$/a\ \ \ \ '"$hs"
    stack build &>/dev/null
    stack run >| "$p.tmp" || return 1
    popd &>/dev/null
    local n=`grep -n '^main.::' "$p.tmp" | awk -F: '{ print $1 }'`
    let n++
    cat "$p.tmp" |
    sed -e '1i\
module\ Main\ where\
\
import GHC.Base\
import GHC.Num\
import\ System.Exit\ (exitFailure)\
import\ System.IO\ (hPutStrLn,\ stderr)\
import\ Inp_gUgVwYdD8r\
import\ Out_gUgVwYdD8r\
' \
        -e '/^main.::/amain\ =\ do' \
        -e "$n~1s/^/    /" >| "$q/app/Main.hs"
    dotarrowScalaToHaskell0 "$2"
    sed -i "$q.json" -e '/^[}]/i,"dummy_gUgVwYdD8r":null'
    return 0
}

function dotarrowHaskellToScala() {
    [ $# -eq 2 ] || return
    local x=dotarrow
    local app="../$x/hs2sc"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local q="`readlink -m \"$x/tmp/$2\"`"
    local r="`readlink -m \"$x/src/$2\"`"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/hs2sc" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/hs2sc" &>/dev/null
    stack build &>/dev/null
    stack run -- "$q/app/Main.hs" >| "$r.scala.src"
    popd &>/dev/null
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -i "$r.scala.src"
    amm -c 'import $ivy.`org.scalameta:scalameta_2.13:4.9.6`;
            import scala.meta._; import dialects.Scala3;
            print('"`cat \"$r.scala.src\"`"')' 2>/dev/null >| "$x/$2.scala"
    sed -i "$q.json" -e '/dummy_gUgVwYdD8r/d'
    return 0
}

function dotarrowAeson0() {
    local x=dotarrow
    local p="`readlink -m \"$x/tmp/$1\"`"
    local _24="`printf '%24s'`"
    local _24="${_24// /\\ }"
    local n=`dotarrowCount "$p.txt"`
    local k=`dotarrowCountLast "$p.txt"`
    let m=n-k
    sed -i "$p/app/Inp_"*.hs -e "10,+${m}d"
    sed -i "$p/app/Out_"*.hs -e "11,+$((m+k))d"
    cat "$p.txt" |
    while read count_name_tpe
    do
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            local c=,
        done
    done
    cat "$p.tmp" | tail -n 1 |
    if read count_name_tpe
    then
        [ $n -eq 0 ] || local c=,
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
            local c=,
        done
    fi
    [ $n -eq 0 ] || local c=,
    sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
    sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24},\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
}

function dotarrowAeson() {
    [ $# -le 2 ] || return
    local x=dotarrow
    local app="../$x/${2:-.}/aeson"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local n=`cat "$p/app/Main.hs" | grep -n '^\\s*import' | awk -F: '{ print $1 }' | sort -n | tail -n 1`
    cat "$p/app/Main.hs" | head -n $n >| "$p.tmp"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/aeson" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/aeson/app" &>/dev/null
    mv Main.hs{.in,}
    local _70="`printf '%70s'`"
    local _70="${_70// /\\ }"
    local _57="`printf '%57s'`"
    local _57="${_57// /\\ }"
    cat "$p.txt" |
    while read count_name_tpe
    do
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i Main.hs \
                -e "/WildP/i${_70}${c:-\\ }VarP\ (Name\ (OccName\ \"$name\")\ NameS)" \
                -e "/[]].[+][+].ns/i${_57}${c:-\\ }\"$name\""
            local c=,
        done
    done
    if [ -s "$p.txt" ]
    then
        sed -i Main.hs -e "s/.WildP/,WildP/"
    fi
    stack build &>/dev/null
    stack run -- "$p/app/Main.hs" >> "$p.tmp" || return 1
    popd &>/dev/null
    sed -i "$p.tmp" \
        -e "/itmp.XXXXXXXXXX/s|itmp.XXXXXXXXXX|$p.json|" \
        -e "/otmp.XXXXXXXXXX/s|otmp.XXXXXXXXXX|$p.json.out|"
    dotarrowAeson0 "$1"
    cat "$p.tmp" | head -n -1 |
    sed -e 's/GHC.\(Num\|Types\).//g' >| "$p/app/Main.hs"
    rm "$p.tmp" &>/dev/null
    pushd "$p/app" &>/dev/null
    stack build &>/dev/null
    stack run 3>&1 1>&2- 2>&3- | sed -e 's/[ ]/\\ /g' >> "$p.txt" || return 1
    popd &>/dev/null
    mv "$p.json"{.out,}
    return 0
}

function dotarrowAeson2() {
    [ $# -le 2 ] || return
    local x=dotarrow
    local app="../$x/${2:-.}/aeson2"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local n=`cat "$p/app/Main.hs" | grep -n '^\\s*import' | awk -F: '{ print $1 }' | sort -n | tail -n 1`
    cat "$p/app/Main.hs" | head -n $n >| "$p.tmp"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/aeson2" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/aeson2/app" &>/dev/null
    mv Main.hs{.in,}
    cat "$p.txt" | tail -n 1 |
    if read count_name_tpe
    then
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        local _70="`printf '%70s'`"
        local _70="${_70// /\\ }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i Main.hs -e "/[]].ss\$/i${_70}${c:-\\ }(\"$name\",\ \"$tpe\")"
            local c=,
        done
    fi
    stack build &>/dev/null
    stack run -- "$p/app/Main.hs" >> "$p.tmp" || return 1
    popd &>/dev/null
    cat "$p.tmp" |
    sed -e 's/GHC.\(Num\|Types\).//g' >| "$p/app/Main.hs"
    rm "$p.tmp" &>/dev/null
    return 0
}

function dotarrowCirce() {
    [ $# -le 2 ] || return
    local x=dotarrow
    local app="../$x/${2:-.}/circe/app.scala.in"
    local n=`cat "$app" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    cat "$app" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.scala.src" |
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -e 's/Init.Type.Apply.Type.Name."Numeric".,.Type.ArgClause.List.Type.Name."Encoding"....,.Name.Anonymous..,.Nil./Init(Type.Apply(Type.Name("Numeric"), Type.ArgClause(List(Type.Name("Encoding")))), Name.Anonymous(), Seq())/g' \
        -e 's/Ctor.Primary.Nil,.Name.Anonymous..,.Nil./Ctor.Primary(Nil, Name.Anonymous(), Seq())/g' \
        -e 's/^/    /' >> "$x/tmp/$1.tmp"
    cat "$app" | tail -n +`expr $n + 1` |
    sed -e '/[io]tmp.XXXXXXXXXX/s/\([io]\)tmp.XXXXXXXXXX/\1'"$1/g" \
        -e "/\"tmp.XXXXXXXXXX\"/s|[\"]tmp.XXXXXXXXXX[\"]|\"$x/tmp/$1.json\"|g" >> "$x/tmp/$1.tmp"
    cat "$x/tmp/$1.txt" |
    while read count_name_tpe
    do
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$x/tmp/$1.tmp" \
                -e "/val.ls:.List.Stat./aDefn.Var(Nil,List(Pat.Var(Term.Name(\"$name\"))),None,Term.ApplyType(Term.Select(Lit.Null(),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
                -e "/Defn.Class.*[io]$1/aTerm.Param(Nil,Term.Name(\"$name\"),Some(Type.Name(\"$tpe\")),None)," \
                -e "/Defn.Val.*i$1/aTerm.Assign(Term.Name(\"$name\"),Term.Select(Term.Name(\"json\"),Term.Name(\"$name\")))," \
                -e "/Defn.Val.*o$1/aTerm.Name(\"$name\"),"
        done
    done
    mv "$x/tmp/$1.tmp" "$x/$1.scala"
    rm "$x/src/$1.scala.src" &>/dev/null
}

function dotarrowCirce2() {
    [ $# -le 2 ] || return
    local x=dotarrow
    local app2="../$x/${2:-.}/circe/app2.scala.in"
    local n=`cat "$app2" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    cat "$app2" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.scala.src" |
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -e 's/Init.Type.Apply.Type.Name."Numeric".,.Type.ArgClause.List.Type.Name."Encoding"....,.Name.Anonymous..,.Nil./Init(Type.Apply(Type.Name("Numeric"), Type.ArgClause(List(Type.Name("Encoding")))), Name.Anonymous(), Seq())/g' \
        -e 's/Ctor.Primary.Nil,.Name.Anonymous..,.Nil./Ctor.Primary(Nil, Name.Anonymous(), Seq())/g' \
        -e 's/^/    /' >> "$x/tmp/$1.tmp"
    cat "$app2" | tail -n +`expr $n + 1` >> "$x/tmp/$1.tmp"
    cat "$x/tmp/$1.txt" | tail -n 1 |
    if read count_name_tpe
    then
        local count="${count_name_tpe%% *}"
        local name_tpe="${count_name_tpe#* }"
        while [ $count -gt 0 ]
        do
            let count--
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            if [ $count -gt 0 ]
            then
                local name_tpe="${tpe#* }"
                local tpe="${tpe%% *}"
            fi
            sed -i "$x/tmp/$1.tmp" \
                -e "/names\s*=\s*List/a\"$name\"," \
                -e "/tpes\s*=\s*List/a\"$tpe\","
        done
    fi
    mv "$x/tmp/$1.tmp" "$x/$1.scala"
    rm "$x/src/$1.scala.src" &>/dev/null
}

function dotarrowStream() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app="../$x/stream/app.scala.in"
    local n=`cat "$app" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    cat "$app" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.src" |
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -e 's/^/    /' >> "$x/tmp/$1.tmp"
    cat "$app" | tail -n +`expr $n + 1` |
    sed -e "/tmp.XXXXXXXXXX/s|tmp.XXXXXXXXXX|$x/tmp/$1.bin|g" >> "$x/tmp/$1.tmp"
    tac "$x/tmp/$1.txt" |
    while read name_tpe
    do
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i "$x/tmp/$1.tmp" \
            -e "/val.ls:.List.Stat./aDefn.Var(Nil,List(Pat.Var(Term.Name(\"$name\"))),None,Term.ApplyType(Term.Select(Lit.Null(),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
            -e "/readBoolean/aTerm.Assign(Term.Name(\"$name\"),Term.ApplyType(Term.Select(Term.Apply(Term.Select(Term.Name(\"ois\"),Term.Name(\"readObject\")),Term.ArgClause(Nil,None)),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
            -e "/writeBoolean/aTerm.Apply(Term.Select(Term.Name(\"oos\"),Term.Name(\"writeObject\")),Term.ArgClause(List(Term.Name(\"$name\")),None)),"
    done
}

function dotarrowStream2() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app2="../$x/stream/app2.scala.in"
    local n=`cat "$app2" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    local name_tpe="`tac \"$x/tmp/$1.txt\" | head -n 1`"
    local name="${name_tpe%% *}"
    local tpe="${name_tpe#* }"
    cat "$app2" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.src" |
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -e 's/^/    /' >> "$x/tmp/$1.tmp"
    cat "$app2" | tail -n +`expr $n + 1` |
    sed -e "/name[ ]*:[ ]*String/s/\$/\"$name\"/" \
        -e "/tpe[ ]*:[ ]*String/s/\$/\"$tpe\"/" >> "$x/tmp/$1.tmp"
}

export -f dotarrowCount dotarrowCountPreLast dotarrowCountLast
export -f dotarrowScalaToHaskell0 dotarrowScalaToHaskell dotarrowHaskellToScala
export -f dotarrowAeson0 dotarrowAeson dotarrowAeson2
export -f dotarrowCirce dotarrowCirce2 dotarrowStream dotarrowStream2
export -f pio pi pi_
