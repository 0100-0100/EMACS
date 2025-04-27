# --- General Colors ---
RST_COLOR=$'%{\033[0m%}'
BAD_COLOR=$'%{\033[38;5;196m%}'
DIR_COLOR=$'%{\033[38;5;246m%}'
OK__COLOR=$'%{\033[38;5;49m%}'
SYM_COLOR=$'%{\033[38;5;220m%}'

# --- Git Status Colors.
ADDED_____COLOR=$'%{\033[38;5;46m%}'
COMMITS___COLOR=$'%{\033[38;5;69m%}'
DELETED___COLOR=$'%{\033[38;5;196m%}'
MODIFIED__COLOR=$'%{\033[38;5;214m%}'
RENAMED___COLOR=$'%{\033[38;5;33m%}'
STASH_____COLOR=$'%{\033[38;5;200m%}'
UNTRACKED_COLOR=$'%{\033[38;5;250m%}'

# --- Unicode Symbols ---
SYM_UNTRACKED="?"
SYM__MODIFIED="~"
SYM___DELETED="-"
SYM___RENAMED="→"
SYM_____ADDED="+"
SYM_____STASH="≡"
SYM______DOWN="↓"
SYM________UP="↑"

# --- Functions ---
parse_git_branch() {
    local added=0 modified=0 deleted=0 untracked=0 renamed=0 ahead=0 behind=0 stash_count=0
    local dir_len space_str output branch git_status
    local in_git_repo=false
    local -a stash_list
    stash_list=("${(f)$(git stash list 2>/dev/null)}")
    stash=${#stash_list[@]}
    dir_len=${#PWD##*/}
    space_str=$(printf ' %.0s' $(seq 0 $((dir_len + 2))))
    git_status=("${(@f)$(git status --porcelain=2 --branch 2>/dev/null)}")
    if [[ -n "$git_status" ]]; then
        in_git_repo=true
    fi

    if $in_git_repo; then

        for line in "${git_status[@]}"; do
            # In the case a file was added and also modified this will catch it and add to both counts
            # A case statement would not.
            [[ "$line" == "# branch.head "* ]]          && branch="${line#"# branch.head "}"
            [[ "$line" == "# branch.ab "* ]] && {
                local ahead_behind
                ahead_behind=(${(s: :)${line#"# branch.ab "}})
                [[ $ahead_behind[1] == +* ]] && ahead=${ahead_behind[1]#\+}
                [[ $ahead_behind[2] == -* ]] && behind=${ahead_behind[2]#-}
            }
            [[ "$line" =~ "^1\ A.+|^1\ M.+" ]]          && (( added++ ));
            [[ "$line" =~ "^1\ .M.+" ]]                 && (( modified++ ));
            [[ "$line" =~ "^1\ \.D.+" ]]                && (( deleted++));
            [[ "$line" =~ "^\?.+" ]]                    && (( untracked++ ));
            [[ "$line" =~ "^2\ .+" ]]                   && (( renamed++ ));
        done

        output=""
        [[ $added     -gt 0 ]] && output+="${ADDED_____COLOR}${SYM_____ADDED}${added} "
        [[ $modified  -gt 0 ]] && output+="${MODIFIED__COLOR}${SYM__MODIFIED}${modified} "
        [[ $deleted   -gt 0 ]] && output+="${DELETED___COLOR}${SYM___DELETED}${deleted} "
        [[ $untracked -gt 0 ]] && output+="${UNTRACKED_COLOR}${SYM_UNTRACKED}${untracked} "
        [[ $renamed   -gt 0 ]] && output+="${RENAMED___COLOR}${SYM___RENAMED}${renamed} "
        [[ $ahead     -gt 0 ]] && output+="${COMMITS___COLOR}${SYM________UP}${ahead} "
        [[ $behind    -gt 0 ]] && output+="${COMMITS___COLOR}${SYM______DOWN}${behind} "
        [[ $stash     -gt 0 ]] && output+="${STASH_____COLOR}${SYM_____STASH}${stash} "

        # If Python virtual environment.
        if [ -n "$VIRTUAL_ENV" ]; then
            echo "%{${space_str}${SYM_COLOR}╭─ ${OK__COLOR}${branch}${RST_COLOR} ${output}${RST_COLOR}\r${OK__COLOR}(venv)${RST_COLOR}\n\r%}"
        else
            echo "%{${space_str}${SYM_COLOR}╭─ ${OK__COLOR}${branch}${RST_COLOR} ${output}${RST_COLOR}\n\r%}"
        fi
    fi
}

set_prompt() {
    local git_info prompt_symbol

    git_info=$(parse_git_branch)
    if [[ $EUID -eq 0 ]]; then
        prompt_symbol="${BAD_COLOR} %#${RST_COLOR}"
    else
        prompt_symbol="${OK__COLOR} %#${RST_COLOR}"
    fi
    PROMPT="${git_info}${DIR_COLOR}%1~${prompt_symbol} ${SYM_COLOR}│${RST_COLOR} "
}

precmd_functions=(set_prompt)
