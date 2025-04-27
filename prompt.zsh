setopt PROMPT_SUBST
autoload -U colors && colors

# --- Colors ---
RESET="%{\033[0m%}"

DIR_COLOR="%{\033[38;5;246m%}"
SYM_COLOR="%{\033[38;5;220m%}"
OK_COLOR="%{\033[38;5;49m%}"
BAD_COLOR="%{\033[38;5;196m%}"

ADDED_COLOR="%{\033[38;5;46m%}"
MODIFIED_COLOR="%{\033[38;5;214m%}"
DELETED_COLOR="%{\033[38;5;196m%}"
UNTRACKED_COLOR="%{\033[38;5;250m%}"
RENAMED_COLOR="%{\033[38;5;33m%}"
HEAD_COLOR="%{\033[38;5;69m%}"

# --- Unicode Symbols ---
SYM_ADDED="+"
SYM_MODIFIED="~"
SYM_DELETED="-"
SYM_UNTRACKED="?"
SYM_RENAMED="→"
SYM_UP="↑"
SYM_DOWN="↓"

# --- Functions ---
parse_git_branch() {
    local dir_len space_str output branch git_status in_git_repo=false added=0 modified=0 deleted=0 untracked=0 renamed=0 ahead=0 behind=0
    dir_len=${#PWD##*/}
    space_str=$(printf ' %.0s' $(seq 0 $((dir_len + 2))))

    git_status=("${(@f)$(git status --porcelain=2 --branch 2>/dev/null)}")
    if [[ -n "$git_status" ]]; then
        in_git_repo=true
    fi

    if $in_git_repo; then

        # Parse branch name from the first line
        for line in "${git_status[@]}"; do
            if [[ "$line" == "# branch.head "* ]]; then
                branch="${line#"# branch.head "}"
                break
            fi
        done

        # Parse status entries
        for line in "${git_status[@]}"; do
            case "$line" in
                (\# branch.ab*ahead*)
                    [[ "$line" =~ ahead[[:space:]]([0-9]+) ]] && ahead=${match[1]}
                    [[ "$line" =~ behind[[:space:]]([0-9]+) ]] && behind=${match[1]}
                    ;;
                (1\ M\.*)
                    (( added++ ))
                    ;;
                (1\ \.M*)
                    (( modified++ ))
                    ;;
                (1\ \.D*)
                    (( deleted++ ))
                    ;;
                (\?*)
                    (( untracked++ ))
                    ;;
                (2\ *)
                    (( renamed++ ))
                    ;;
            esac
        done

        output=""
        [[ $added     -gt 0 ]] && output+="${ADDED_COLOR}${SYM_ADDED}${added} "
        [[ $modified  -gt 0 ]] && output+="${MODIFIED_COLOR}${SYM_MODIFIED}${modified} "
        [[ $deleted   -gt 0 ]] && output+="${DELETED_COLOR}${SYM_DELETED}${deleted} "
        [[ $untracked -gt 0 ]] && output+="${UNTRACKED_COLOR}${SYM_UNTRACKED}${untracked} "
        [[ $renamed   -gt 0 ]] && output+="${RENAMED_COLOR}${SYM_RENAMED}${renamed} "
        [[ $ahead     -gt 0 ]] && output+="${HEAD_COLOR}${SYM_UP}${ahead} "
        [[ $behind    -gt 0 ]] && output+="${HEAD_COLOR}${SYM_DOWN}${behind} "

        if [ -n "$VIRTUAL_ENV" ]; then
            echo "%{${space_str}${SYM_COLOR}╭─ ${OK_COLOR}${branch}${RESET} ${output}${RESET}\r${OK_COLOR}(venv)${RESET}\n\r%}"
        else
            # echo "No virtual environment is active."
            echo "%{${space_str}${SYM_COLOR}╭─ ${OK_COLOR}${branch}${RESET} ${output}${RESET}\n\r%}"
        fi
        # echo "%{${space_str}${SYM_COLOR}╭─ ${OK_COLOR}${branch}${RESET} ${output}${RESET}\n\r%}"
    fi
}

set_prompt() {
    local git_info prompt_symbol

    git_info=$(parse_git_branch)
    # PROMPT="${git_info}%1~ │ "
    # PS1="${git_info}%1~ │ "
    # PS1="${git_info}${DIR_COLOR}%1~ ${prompt_symbol} ${SYM_COLOR}│${RESET} "
    # PROMPT="${git_info}${DIR_COLOR}%1~ ${prompt_symbol} ${SYM_COLOR}│${RESET} "
    PROMPT="${git_info}"
    PROMPT+=$'%{\033[38;5;246m%}'
    PROMPT+="%1~ "
    PROMPT+=$'%{\033[0m%}'
    if [[ $EUID -eq 0 ]]; then
        PROMPT+=$'%{\033[38;5;196m%}'
        PROMPT+="%#"
        PROMPT+=$'%{\033[0m%}'
    else
        PROMPT+=$'%{\033[38;5;49m%}'
        PROMPT+="%#"
        PROMPT+=$'%{\033[0m%}'
    fi
    PROMPT+=$'%{\033[38;5;220m%}'
    PROMPT+=" │ "
    PROMPT+=$'%{\033[0m%}'
    # (OPTIONAL) RPROMPT="HELLLO!!!"
}

precmd_functions=(set_prompt)
