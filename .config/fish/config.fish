if status is-interactive
set fish_greeting ""

alias cp "cp -i"
alias mv "mv -i"
alias rm "rm -i"
alias vi "nvim"
alias vim "nvim"
alias mutt "neomutt"
alias todos "grep -nRI TODO"

set -x GPG_TTY (tty)

bind \e accept-autosuggestion

set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block

starship init fish | source
end
