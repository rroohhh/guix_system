function __prompt_last_exit_code {
  [[ $last_exit_code -gt 0 ]] || return 1;

  printf "%s" "$last_exit_code"
}

function __prompt_ps1 {
  local slice_prefix slice_empty_prefix slice_joiner slice_suffix is_prompt_empty=1

  local last_exit_code=''
  [[ $exit_code -gt 0 ]] && last_exit_code="$exit_code"

  local host=''
  [ ! -z "${SSH_CLIENT}" ] && host='\h'

  __prompt_wrapper "$host" "${a_fg}${a_bg}${space}" "${space}${a_sep_fg}" && is_prompt_empty=0

  if [ "$(id -u)" -eq 0 ]; then
      [ $is_prompt_empty -eq 1 ] && slice_prefix="${d_fg}${d_bg}${space}" || slice_prefix="${d_bg}${sep}${b_fg}${b_bg}${space}"
      __prompt_wrapper "ROOT" "$slice_prefix" "${space}${d_sep_fg}" && is_prompt_empty=0
  fi

  [ $is_prompt_empty -eq 1 ] && slice_prefix="${c_fg}${c_bg}${space}" || slice_prefix="${c_bg}${sep}${c_fg}${space}"
  __prompt_wrapper "$(__prompt_cwd)" "$slice_prefix" "${space}${c_sep_fg}" && is_prompt_empty=0

  if [ -n "${GUIX_ENVIRONMENT}" ]; then
      [ $is_prompt_empty -eq 1 ] && slice_prefix="${b_fg}${b_bg}${space}" || slice_prefix="${b_bg}${sep}${b_fg}${b_bg}${space}"
      __prompt_wrapper "ENV" "$slice_prefix" "${space}${b_sep_fg}" && is_prompt_empty=0
  fi


  __prompt_wrapper "$(__prompt_vcs_branch)" "${y_bg}${sep}${y_fg}${space}" "${space}${y_sep_fg}"

  __prompt_wrapper "$last_exit_code" "${warn_bg}${sep}${warn_fg}${space}" "${space}${warn_sep_fg}"

  # close sections
  printf "%s" "${reset_bg}${sep}$reset$space"
}
function __prompt_vcs_branch {
  local branch
  local branch_symbol=" "

  # git
  if hash git 2>/dev/null; then
    if branch=$( { git symbolic-ref --quiet HEAD || git rev-parse --short HEAD; } 2>/dev/null ); then
      branch=${branch##*/}
      printf "%s" "${branch_symbol}${branch:-unknown}"
      return
    fi
  fi
  return 1
}

function __prompt_cwd {
  local dir_limit="3"
  local truncation="⋯"
  local first_char
  local part_count=0
  local formatted_cwd=""
  local dir_sep="  "
  local tilde="~"

  local store="/gnu/store/????????????????????????????????"
  local store_repl="/gnu/store/…"
  # local store_repl="/gnu/store/..."
  local cwd="${PWD/#$HOME/$tilde}"
  cwd="${cwd/#$store/$store_repl}"

  # get first char of the path, i.e. tilde or slash
  first_char=${cwd::1}

  # remove leading tilde
  cwd="${cwd#\~}"

  while [[ "$cwd" == */* && "$cwd" != "/" ]]; do
    # pop off last part of cwd
    local part="${cwd##*/}"
    cwd="${cwd%/*}"

    formatted_cwd="$dir_sep$part$formatted_cwd"
    part_count=$((part_count+1))

    [[ $part_count -eq $dir_limit ]] && first_char="$truncation" && break
  done

  printf "%s" "$first_char$formatted_cwd"
}

function __prompt_wrapper {
  # wrap the text in $1 with $2 and $3, only if $1 is not empty
  # $2 and $3 typically contain non-content-text, like color escape codes and separators

  [[ -n "$1" ]] || return 1
  printf "%s" "${2}${1}${3}"
}

function __prompt {
  local exit_code="$?"
  local esc=$'[' end_esc=m
  local noprint='\[' end_noprint='\]'
  local wrap="$noprint$esc" end_wrap="$end_esc$end_noprint"
  local space=" "
  local sep=""
  local rsep=""
  local alt_sep=""
  local alt_rsep=""
  local reset="${wrap}0${end_wrap}"
  local reset_bg="${wrap}49${end_wrap}"
  local a_fg="${wrap}38;5;235${end_wrap}"
  local a_bg="${wrap}48;5;246${end_wrap}"
  local a_sep_fg="${wrap}38;5;246${end_wrap}"
  local b_bg="${wrap}48;5;2${end_wrap}"
  local b_fg="${wrap}38;5;235${end_wrap}"
  local b_sep_fg="${wrap}38;5;2${end_wrap}"
  local c_fg="${wrap}38;5;246${end_wrap}"
  local c_bg="${wrap}48;5;237${end_wrap}"
  local c_sep_fg="${wrap}38;5;237${end_wrap}"
  local d_bg="${wrap}48;5;1${end_wrap}"
  local d_fg="${wrap}38;5;235${end_wrap}"
  local d_sep_fg="${wrap}38;5;1${end_wrap}"
  local warn_fg="${wrap}38;5;235${end_wrap}"
  local warn_bg="${wrap}48;5;208${end_wrap}"
  local warn_sep_fg="${wrap}38;5;208${end_wrap}"
  local y_fg="${wrap}38;5;246${end_wrap}"
  local y_bg="${wrap}48;5;239${end_wrap}"
  local y_sep_fg="${wrap}38;5;239${end_wrap}"

  PS1="$(__prompt_ps1)"
}

if [[ ! "$PROMPT_COMMAND" == *__prompt* ]]; then
    PROMPT_COMMAND='__prompt;'
fi
