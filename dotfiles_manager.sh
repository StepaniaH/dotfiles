#!/usr/bin/env bash

DOTFILES_DIR="$HOME/Developer/dotfiles"
TARGET_DIR="$HOME/"
BACKUP_DIR="$HOME/dotfiles-backup/$(date +%Y%m%d_%H%M%S)"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Check the OS
OS_TYPE=""
if [[ "$(uname)" == "Darwin" ]]; then
    OS_TYPE="macos"
    echo -e "${BLUE}This is the macOS${NC}"
elif [[ -f "/etc/arch-release" ]]; then
    OS_TYPE="arch"
    echo -e "${BLUE}This is the Arch Linux${NC}"
else
    echo -e "${RED}Unknow OS, seem it as Linux${NC}"
    OS_TYPE="linux"
fi

echo -e "${GREEN}Start connecting dotfiles...${NC}"
echo -e "${YELLOW}Backup Dir: $BACKUP_DIR${NC}"

# Backup file -- make sure the file's location
backup_file() {
    local file_path="$1"
    local relative_path="${file_path#$TARGET_DIR}"
    local backup_path="$BACKUP_DIR$relative_path"
    local backup_dir="$(dirname "$backup_path")"

    # Create backup target directory (if it does not exist)
    if [[ ! -d "$backup_dir" ]]; then
        mkdir -p "$backup_dir"
    fi

    # Copy files or directories to the backup location
    cp -r "$file_path" "$backup_path"
    echo -e "${YELLOW}Backup finished: $file_path -> $backup_path${NC}"
}

create_link() {
    local src="$1"
    local dest="$2"

    # Check the source file is existing or not
    if [[ ! -e "$src" ]]; then
        echo -e "${RED}Error: The source file is missing: $src${NC}"
        return 1
    fi

    # Make sure the target dir is existing
    local dest_dir="$(dirname "$dest")"
    if [[ ! -d "$dest_dir" ]]; then
        mkdir -p "$dest_dir"
        echo -e "${BLUE}Create dir: $dest_dir${NC}"
    fi

    # If the target already exists and is not a link, back it up.
    if [[ -e "$dest" && ! -L "$dest" ]]; then
        echo -e "${YELLOW}Copy the execting files: $dest${NC}"
        backup_file "$dest"
        rm -rf "$dest"
    elif [[ -L "$dest" ]]; then
        echo -e "${YELLOW}Remove existing links: $dest${NC}"
        rm -f "$dest"
    fi

    # Create symbal link
    ln -s "$src" "$dest"
    echo -e "${GREEN}Connecting finished: $src -> $dest${NC}"
}

link_directory_contents() {
    local src_dir="$1"
    local dest_dir="$2"

    # Check the source file is execting or not
    if [[ ! -d "$src_dir" ]]; then
        echo -e "${RED}Error: The source file is missing: $src_dir${NC}"
        return 1
    fi

    # Create backup target directory (if it does not exist)
    if [[ ! -d "$dest_dir" ]]; then
        mkdir -p "$dest_dir"
        echo -e "${BLUE}Create dir: $dest_dir${NC}"
    fi

    echo -e "${BLUE}Link the contents of $src_dir to $dest_dir${NC}"

    # Traverse all files in the source directory, ignoring subdirectories
    for item in "$src_dir"/*; do
        # Only process regular files, ignore directories
        if [[ -f "$item" ]]; then
            local item_name=$(basename "$item")
            local dest_path="$dest_dir/$item_name"

            # If the target already exists and is not a link, back it up.
            if [[ -e "$dest_path" && ! -L "$dest_path" ]]; then
                echo -e "${YELLOW}Backup existing files: $dest_path${NC}"
                backup_file "$dest_path"
                rm -rf "$dest_path"
            elif [[ -L "$dest_path" ]]; then
                echo -e "${YELLOW}Remove execting links: $dest_path${NC}"
                rm -f "$dest_path"
            fi

            # Create symbolic links
            ln -s "$item" "$dest_path"
            echo -e "${GREEN}Connecting finished: $item -> $dest_path${NC}"
        elif [[ -d "$item" ]]; then
            echo -e "${BLUE}Skipping directory: $(basename "$item")${NC}"
        fi
    done
}

# Universal Configuration File (Linked to all systems)
echo -e "${BLUE}Linking general configuration file...${NC}"

## bat
create_link "$DOTFILES_DIR/config/bat/config" "$TARGET_DIR/.config/bat/config"
link_directory_contents "$DOTFILES_DIR/config/bat/themes" "$TARGET_DIR/.config/bat/themes"

## btop
create_link "$DOTFILES_DIR/config/btop/btop.conf" "$TARGET_DIR/.config/btop/btop.conf"
link_directory_contents "$DOTFILES_DIR/config/btop/themes" "$TARGET_DIR/.config/btop/themes"

## emacs
create_link "$DOTFILES_DIR/config/emacs/early-init.el" "$TARGET_DIR/.config/emacs/early-init.el"
create_link "$DOTFILES_DIR/config/emacs/init.el" "$TARGET_DIR/.config/emacs/init.el"
create_link "$DOTFILES_DIR/config/emacs/.mc-lists.el" "$TARGET_DIR/.config/emacs/.mc-lists.el"
link_directory_contents "$DOTFILES_DIR/config/emacs/lisp" "$TARGET_DIR/.config/emacs/lisp"
link_directory_contents "$DOTFILES_DIR/config/emacs/lisp/lang" "$TARGET_DIR/.config/emacs/lisp/lang"

## eza
create_link "$DOTFILES_DIR/config/eza/theme.yml" "$TARGET_DIR/.config/eza/theme.yml"

## fastfetch
create_link "$DOTFILES_DIR/config/fastfetch/cat.txt" "$TARGET_DIR/.config/fastfetch/cat.txt"
create_link "$DOTFILES_DIR/config/fastfetch/config.jsonc" "$TARGET_DIR/.config/fastfetch/config.jsonc"

## glow
create_link "$DOTFILES_DIR/config/glow/glow.yml" "$TARGET_DIR/.config/glow/glow.yml"

## kitty
link_directory_contents "$DOTFILES_DIR/config/kitty" "$TARGET_DIR/.config/kitty"

## lazygit
create_link "$DOTFILES_DIR/config/lazygit/config.yml" "$TARGET_DIR/.config/lazygit/config.yml"

## nvim
create_link "$DOTFILES_DIR/config/nvim/init.lua" "$TARGET_DIR/.config/nvim/init.lua"
link_directory_contents "$DOTFILES_DIR/config/nvim/after/ftplugin" "$TARGET_DIR/.config/nvim/after/ftplugin"
link_directory_contents "$DOTFILES_DIR/config/nvim/lua/config" "$TARGET_DIR/.config/nvim/lua/config"
link_directory_contents "$DOTFILES_DIR/config/nvim/lua/plugins" "$TARGET_DIR/.config/nvim/lua/plugins"
link_directory_contents "$DOTFILES_DIR/config/nvim/plugin" "$TARGET_DIR/.config/nvim/plugin"

## thefuck
create_link "$DOTFILES_DIR/config/thefuck/settings.py" "$TARGET_DIR/.config/thefuck/settings.py"
link_directory_contents "$DOTFILES_DIR/config/thefuck/rules" "$TARGET_DIR/.config/thefuck/rules"

## yazi
link_directory_contents "$DOTFILES_DIR/config/yazi" "$TARGET_DIR/.config/yazi"

## zsh
create_link "$DOTFILES_DIR/config/zsh/.zimrc" "$TARGET_DIR/.config/zsh/.zimrc"
link_directory_contents "$DOTFILES_DIR/config/zsh" "$TARGET_DIR/.config/zsh"

# The macOS's files
if [ $OS_TYPE == "macos" ]
then
  ## karabiner
  create_link "$DOTFILES_DIR/config/karabiner/karabiner.json" "$TARGET_DIR/.config/karabiner/karabiner.json"
  link_directory_contents "$DOTFILES_DIR/config/yabai" "$TARGET_DIR/.config/yabai"
fi

# The Arch OS's files.
if [ $OS_TYPE == "arch" ]
then
  # dunst
  create_link "$DOTFILES_DIR/config/dunst/dunstrc" "$TARGET_DIR/.config/dunst/dunstrc"

  # environment.d
  create_link "$DOTFILES_DIR/config/environment.d/fcitx5.conf" "$TARGET_DIR/.config/environment.d/fcitx5.conf"

  # fcitx5
  create_link "$DOTFILES_DIR/config/fcitx5/config" "$TARGET_DIR/.config/fcitx5/config"
  create_link "$DOTFILES_DIR/config/fcitx5/conf/classicui.conf" "$TARGET_DIR/.config/fcitx5/conf/classicui.conf"

  # hypr
  link_directory_contents "$DOTFILES_DIR/config/hypr" "$TARGET_DIR/.config/hypr"

  # rofi
  create_link "$DOTFILES_DIR/config/rofi/config.rasi" "$TARGET_DIR/.config/rofi/config.rasi"

  # uwsm
  link_directory_contents "$DOTFILES_DIR/config/uwsm" "$TARGET_DIR/.config/uwsm"

  # waybar
  link_directory_contents "$DOTFILES_DIR/config/waybar" "$TARGET_DIR/.config/waybar"

  # wlogout
  link_directory_contents "$DOTFILES_DIR/config/wlogout" "$TARGET_DIR/.config/wlogout"
  link_directory_contents "$DOTFILES_DIR/config/wlogout/icons" "$TARGET_DIR/.config/wlogout/icons"

  # xremap
  create_link "$DOTFILES_DIR/config/xremap/config.yml" "$TARGET_DIR/.config/xremap/config.yml"
fi
