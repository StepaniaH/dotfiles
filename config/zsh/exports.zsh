# All environment variables

# ZIMFW
export ZDOTDIR="${HOME}/.config/zsh"
ZIM_HOME="${ZDOTDIR}/zim"

if [[ ! -e ${ZIM_HOME}/zimfw.zsh ]]; then
  curl -fsSL --create-dirs -o ${ZIM_HOME}/zimfw.zsh \
    https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh
fi

if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZDOTDIR:-${HOME}}/.zimrc ]]; then
  source ${ZIM_HOME}/zimfw.zsh init -q
fi

source ${ZIM_HOME}/init.zsh


# Basic Settings
export EDITOR="nvim"
export TERMINAL="kitty"
export BROWSER="firefox"
export PATH="$HOME/.local/bin":$PATH
export PATH="$HOME/.docker/bin":$PATH
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export PATH="$HOME/.local/share/neovim/bin":$PATH
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_ENV_HINTS=1
export STARSHIP_CONFIG=~/.config/zsh/starship.toml           # Starship Config
eval "$(starship init zsh)"                              # Starship Init

HISTFILE=~/.config/zsh/.zsh_hist                                     # Where to store zsh history

export EZA_CONFIG_DIR="$HOME/.config/eza"

eval $(thefuck --alias)
eval $(thefuck --alias mario)
