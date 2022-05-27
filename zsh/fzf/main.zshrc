DOTFILES=/Users/john/dotfiles
LB=~/.local/bin

################################################################
#  Environment Variables                                       #
################################################################

export FZF_TMUX=1
export FZF_CTRL_T_OPTS="--preview 'look {}'"
export FZF_ALT_C_OPTS="--preview 'look {}'"
export FZF_DEFAULT_COMMAND='fd'

export FZF_DEFAULT_OPTS="--exact --multi --no-height --layout=reverse-list --cycle --bind=ctrl-k:kill-line,alt-down:page-down,alt-up:page-up"

################################################################
#  Utility Functions                                           #
################################################################

toLower() {
  tr '[A-Z]' '[a-z]'
}

toUpper() {
  tr '[a-z]' '[A-Z]'
}

identityPipe() {
  cat -
}

################################################################
#  Install helper scripts                                      #
################################################################

(/bin/cp -f ${DOTFILES}/zsh/bat_or_exa.zsh     ${LB} &) >/dev/null 2>&1
(/bin/cp -f ${DOTFILES}/zsh/preview-fzf-hn.zsh ${LB} &) >/dev/null 2>&1
(ln -s ${LB}/bat_or_exa.zsh ${LB}/look               &) >/dev/null 2>&1

################################################################
#  Fz Functions                                                #
################################################################

fzbrew() {
  brew list --formula | fzf -m --preview="brew info {}" | xargs brew install
}

wordle() {
  fzwords upper '=5'
}

fzfreveal() {
  fd -t f -H -I | fzf --preview 'bat_or_exa.zsh {}' | xargs open --reveal
}

fzmime() {
  fzf --preview 'file -b --mime-type {}' | xargs file -b --mime-type
}

j() {
    cd "$(z -1 2>&1 | awk '{print $2}' | sort -u | fzf --inline-info --height 33%)"
}

jc() {
    cd "$(z -c -1 2>&1 | awk '{print $2}' | sort -u | fzf --inline-info --height 33%)"
}

fzgitdiff() {
    git status -s | awk '{print $2}' | fzf --preview 'git diff --color=always HEAD -- {}'
}

fzhq() {
  url=$1
  query=$2

  if [ -z "$url" ]; then
     echo "ERROR: No URL provided."
     return
  fi

  if [ -z "$query" ]; then
     echo "ERROR: No QUERY provided."
     return
  fi

  echo "Browsing url: $url"
  echo "   via query: $query"
  curl -s $url \
    | htmlq $query --attribute href            \
    | fzf -m --preview 'lynx -nolist -dump {}' \
    | xargs open
}

# fuzzy sqlite table
fzst() {
  # echo "fst initializing ..."
  sqlite=sqlite3
  # echo sqlite is $sqlite
  db=$(fd '\.sqlite$' | fzf --preview "echo .schema | $sqlite {}")
  # echo DB is $db
  table=$(echo ".tables" | $sqlite $db | xargs -n1 echo | fzf --preview "echo '.schema {}' | $sqlite $db")
  # echo TABLE is $table
  fzql $db "SELECT * FROM $table"
}

exactly() {
  n=$1
  if [ -z "$n" ]; then
    echo "ERROR: exactly(): empty n"
    (cat -)
  else
    awk "length==${n}"
  fi
}

fzwords() {
  filter=identityPipe
  transmogrify=identityPipe

  for arg in $*; do
    case "$arg" in
      lower)    transmogrify=toLower      ;;
      upper)    transmogrify=toUpper      ;;
      identity) transmogrify=identityPipe ;;
      *)        head=$(echo "$arg" | cut -c1)
                tail=$(echo "$arg" | cut -c2-)
                # echo "head is ${head}"
                case "$head" in
                  "+") filter=(sed -r "/^.{0,${tail}}$/d")      ;;
                  "-") filter=(sed -r "/^.{${tail},100000}$/d") ;;
                  "=") filter=(exactly "${tail}")               ;;
                  *)   echo "WARNING: fzwords ignoring unknown argument: ${arg}"
                esac
    esac
  done

  # echo "transmogrify: ${transmogrify}"
  # echo "filter: ${filter}"

  cat /usr/share/dict/words \
      | ${transmogrify}      \
      | ${filter}             \
      | fzf -m
}

fzfgitdiff() {
    git status -s | awk '{print $2}' | fzf --preview 'git diff --color=always HEAD -- {}'
}

fl() {
  url=$1
  query=$2
  if [ -z "$url" ]; then
     echo "ERROR: No URL provided."
     return
  fi
  if [ -z "$query" ]; then
     echo "ERROR: No QUERY provided."
     return
  fi
  echo "Browsing url: $url"
  echo "   via query: $query"
  curl -s $url \
    | htmlq $query --attribute href            \
    | fzf -m --preview 'lynx -nolist -dump {}' \
    | xargs open
}

fst() {
  # echo "fst initializing ..."
  sqlite=sqlite3
  query=$1
  # echo sqlite is $sqlite

  db=$(fd '.sqlite$' | fzf --preview "echo .schema | $sqlite {}")
  # echo DB is $db

  table=$(echo ".tables" | $sqlite $db | xargs -n1 echo | fzf --preview "echo '.schema {}' | $sqlite $db")
  # echo TABLE is $table

  if [ -z "$query" ]; then
    query="SELECT * FROM "
  fi

  # echo "Using query: ${query} ${table}"

  fzql $db "$query $table"
}

# notes_show() {
#    notes_dir="/Users/john/OneDrive\ -\ Interos\ Inc/Obsidian-Vault/John\'s\ Vault"
#    echo "NOTES DIR: [" notes_dir "]"
#    cd "$notes_dir" && fzf -m --preview 'bat {}'  | xargs bat
# }

hn() {

  # echo "PRE TEMPFILE STUFF"
  TMPPREFIX=${TMPDIR:-/tmp}/johns-zshrc.
  TEMP_FILE=$(mktemp)
  TEMP_TITLES=$(mktemp)
  TEMP_LINKS=$(mktemp)
  TEMP_PASTED=$(mktemp)
  # echo TEMP_FILE:   ${TEMP_FILE}
  # echo TEMP_TITLES: ${TEMP_TITLES}
  # echo TEMP_LINKS:  ${TEMP_LINKS}
  # echo TEMP_PASTED: ${TEMP_PASTED}
  # echo "POST TEMPFILE STUFF"

  url="https://news.ycombinator.com/"
  query="a.titlelink"
  curl -s $url> ${TEMP_FILE}
  # ls -l ${TEMP_FILE}
  cat ${TEMP_FILE} | htmlq a.titlelink --text > ${TEMP_TITLES}
  cat ${TEMP_FILE} | htmlq a.titlelink --attribute href > ${TEMP_LINKS}
  paste -d'\t' ${TEMP_TITLES} ${TEMP_LINKS} > ${TEMP_PASTED}         \
     | fzf                                                           \
          -m                                                         \
          -d'\t'                                                     \
          --with-nth=1                                               \
          --nth=2                                                    \
          --preview "preview-fzf-hn.zsh {}"                          \
          --preview-window "right:66%"                               \
     | awk -F'\t' '{print $(NF)}'                                    \
     | sed -E '/^https:/!s/(.*)/https:\/\/news.ycombinator.com\/\1/' \
     | xargs -n1 open
}

unset REPORTTIME  # for now